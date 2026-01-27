# HDF5 Plugin Signature Security Analysis

## Executive Summary

This document analyzes the security architecture of HDF5's plugin signature verification system, documenting known limitations, potential attack vectors, and mitigation strategies.

**Current Status:** Raw RSA signature verification with KeyStore support (January 2026)

**Security Posture:** Adequate for HDF5's threat model and operational constraints

---

## Critical Architectural Analysis

### Overview of Security Model

HDF5 uses **raw RSA signatures** with **KeyStore-based trust management** to verify plugin authenticity:

```
Plugin Security Flow:
1. Developer signs plugin with private key (RSA-2048 + SHA-256)
2. Signature appended to plugin binary with footer metadata
3. At runtime, HDF5 verifies signature against KeyStore (multiple trusted keys)
4. Plugin loads only if signature matches ANY trusted key (OR logic)
```

**Design Philosophy:**
- Zero operational cost ($0/year)
- Zero dedicated staffing (0 FTE)
- Perfect for air-gapped environments
- Distributed trust model (no central authority)

**Trade-off:** Security features vs operational burden

---

## Concern #1: Multi-Vendor Trust (RESOLVED ✅)

### Original Problem (Before KeyStore)

**Issue:** Single public key embedded at compile time

```bash
# Could only trust ONE signer
cmake -DH5PL_PUBLIC_KEY_PEM="$(cat llnl-public.pem)"

# Result:
✓ LLNL-signed plugins accepted
✗ Blosc-signed plugins rejected
✗ HDFGroup-signed plugins rejected
```

**Impact:** Organizations couldn't use plugins from multiple vendors without:
1. Re-signing all plugins with their own key (high burden), OR
2. Building multiple HDF5 installations (impractical), OR
3. Sharing private keys between vendors (security violation)

### Resolution: KeyStore Implementation (January 2026)

**Solution:** Runtime loading of multiple trusted keys from directory

```bash
# Setup KeyStore with multiple trusted organizations
sudo mkdir -p /etc/hdf5/trusted_keys
sudo chmod 755 /etc/hdf5/trusted_keys
sudo cp llnl_public.pem /etc/hdf5/trusted_keys/
sudo cp anl_public.pem /etc/hdf5/trusted_keys/
sudo cp hdfgroup_public.pem /etc/hdf5/trusted_keys/

# Build HDF5 with KeyStore
cmake -DHDF5_REQUIRE_SIGNED_PLUGINS=ON \
      -DHDF5_PLUGIN_KEYSTORE_DIR=/etc/hdf5/trusted_keys

# Result:
✓ LLNL-signed plugins accepted
✓ ANL-signed plugins accepted
✓ HDFGroup-signed plugins accepted
```

**Key Features:**
- **Multiple keys:** Load all .pem files from directory
- **OR logic:** Plugin succeeds if ANY key matches
- **Runtime updates:** Add/remove keys without recompiling
- **Security:** Directory must not be world-writable
- **Backward compatible:** Single embedded key still supported

**Implementation:**
- Location: `src/H5PLsig.c`, `src/H5PLsig.h`
- Design: `KEYSTORE_DESIGN.md`
- Testing: `KEYSTORE_TESTING.md`

**Status:** ✅ **RESOLVED** - Multi-vendor trust is now supported

---

## Concern #2: No Revocation Mechanism (ACCEPTED LIMITATION ⚠️)

### The Problem

**Attack Scenario:**

```
Day 0:   Developer has private key (secure)
         Signs plugin v1.0.0 → valid signature

Day 100: Attacker steals developer's private key
         (via malware, insider threat, compromised CI/CD, etc.)

Day 101: Attacker signs malicious plugin with stolen key
         Signature is cryptographically VALID
         HDF5 library accepts it ✓ (can't detect compromise)

Day 102: Developer discovers key compromise
         Alert sent to all users

Day 103: Response begins (SLOW)
         1. Developer generates new key pair
         2. Developer re-signs all plugins
         3. Users must update KeyStore (remove old key)
         4. Large HPC deployments: weeks to months
```

**Impact:** Window of vulnerability between compromise and remediation

**Current Mitigation:** NONE at the cryptographic level

### Why We Accept This

**Threat Model Assumptions:**

1. **Keys are well-protected:**
   - Stored in HSM or encrypted storage
   - Not exposed to internet
   - Access-controlled (not on developer workstations)
   - Stored as GitHub Secrets / Vault for CI/CD

2. **Detection is fast:**
   - Monitoring of plugin signatures
   - Anomaly detection (unusual signing activity)
   - Community reporting

3. **Distribution is controlled:**
   - Official channels (GitHub releases, organizational mirrors)
   - Not downloaded from random websites
   - HTTPS prevents MITM during download

4. **Limited attack window:**
   - Key compromise is rare (no incidents to date)
   - Attacker must act quickly before detection
   - Limited number of users affected before response

### Mitigation Strategies (Without Full PKI)

#### Option A: Plugin Registry Blocklist

```yaml
# HDFGroup/hdf5-plugin-registry/blocklist.yaml
revoked_keys:
  - fingerprint: "sha256:abc123def456..."
    organization: "LLNL"
    reason: "Key compromised 2026-03-15"
    effective_date: "2026-03-15"
    replacement_key: "sha256:xyz789..."
```

**Implementation:**
```c
// Optional blocklist checking in H5PLsig.c
if (key_is_in_blocklist(key_fingerprint)) {
    H5Epush_error("Key has been revoked");
    return FAIL;
}
```

**Advantages:**
- ✅ Faster than recompiling HDF5
- ✅ Can be distributed via package managers
- ✅ Organizations can fetch periodically

**Disadvantages:**
- ⚠️ Still requires manual updates
- ⚠️ Not real-time (like OCSP)
- ⚠️ Air-gapped systems still have delay

**Status:** Not implemented (would be part of Plugin Registry - Phase 2)

#### Option B: Short-Lived Keys with Rotation

```bash
# Policy: Rotate keys every 6 months
# Organization process:

# January 2026
openssl genrsa -out private-2026-01.pem 2048
openssl rsa -in private-2026-01.pem -pubout -out public-2026-01.pem
# Sign all plugins with 2026-01 key

# July 2026
openssl genrsa -out private-2026-07.pem 2048
openssl rsa -in private-2026-07.pem -pubout -out public-2026-07.pem
# Sign all NEW plugins with 2026-07 key
# Re-sign critical OLD plugins

# Destroy 2026-01 private key
shred -vfz -n 10 private-2026-01.pem
```

**Effect:**
- Old plugins naturally "expire" after rotation period
- Compromise window limited to 6 months
- No revocation infrastructure needed

**Status:** Recommended best practice (documented)

#### Option C: Plugin Timestamping (Future Enhancement)

**Extend footer format:**
```c
typedef struct H5PL_sig_footer_v2_t {
    uint64_t timestamp;         // Unix timestamp when signed
    uint32_t max_age_days;      // Optional: expires after N days
    uint32_t signature_length;
    uint32_t magic;             // 0x48444636 ("HDF6" for v2)
} H5PL_sig_footer_v2_t;
```

**Policy enforcement:**
```c
// Organization sets max plugin age
uint64_t plugin_age = current_time - footer.timestamp;
uint64_t max_age = 180 * 86400;  // 180 days

if (plugin_age > max_age) {
    H5Epush_error("Plugin signature too old");
    return FAIL;
}
```

**Status:** Possible future enhancement (would break backward compatibility)

### When This Becomes Critical

**Escalate to PKI immediately if:**
- ✅ Key compromise incident occurs in HDF5 ecosystem
- ✅ Regulatory requirements mandate revocation capability
- ✅ Insurance/legal requires CRL/OCSP support
- ✅ Multiple security incidents demonstrate risk

**Current assessment:** Risk is acceptable given threat model

---

## Concern #3: No Rollback Protection (ACCEPTED LIMITATION ⚠️)

### The Problem

**Attack Scenario:**

```
Timeline:

Day 0:   Developer releases plugin v1.0.0
         Contains buffer overflow vulnerability (CVE-2026-1234)
         Signed: valid signature ✓

Day 30:  Security researcher discovers vulnerability
         Publicly disclosed

Day 31:  Developer releases plugin v1.0.1
         Vulnerability fixed
         Signed: valid signature ✓

Day 32:  Attacker performs Man-in-the-Middle (MITM) attack
         User requests v1.0.1 download
         Attacker intercepts and serves v1.0.0 instead

         HDF5 verification:
         1. Check signature → VALID (it's the real v1.0.0)
         2. Load plugin → SUCCESS

         User is now running VULNERABLE code
```

**Root Cause:** Signature proves **authenticity** but not **freshness**

**Current Footer:**
```c
typedef struct H5PL_sig_footer_t {
    uint32_t signature_length;  // Only stores length
    uint32_t magic;             // Only stores magic
    // NO VERSION NUMBER
    // NO TIMESTAMP
    // NO MINIMUM VERSION POLICY
} H5PL_sig_footer_t;
```

### Why We Accept This

**Assumptions:**

1. **Trusted distribution channels:**
   - GitHub releases (signed commits, release attestations)
   - Organizational package mirrors
   - HTTPS prevents most MITM attacks
   - Checksums/hashes published alongside releases

2. **Version checking elsewhere:**
   - Applications query plugin version via API
   - Package managers enforce version constraints
   - Organizations maintain approved version lists

3. **Limited attack surface:**
   - Attacker needs network position (not trivial)
   - Window is limited (old versions removed from repos)
   - User awareness (security advisories published)

### Mitigation Strategies

#### Option A: Signed Manifest Files

**Concept:** Distribute metadata alongside plugins

```yaml
# plugin-manifest.yaml (signed)
plugin:
  name: libh5zzfp.so
  version: 1.0.1
  sha256: abc123def456...

security_policy:
  min_valid_version: 1.0.1       # v1.0.0 is blacklisted
  effective_date: 2026-03-15     # Policy enforced after this
  vulnerability: CVE-2026-1234   # Reason for policy

signature: <RSA signature of above>
```

**Verification:**
```c
// Load manifest and verify its signature
manifest = load_manifest("plugin-manifest.yaml");
verify_signature(manifest);

// Check plugin against policy
if (plugin.version < manifest.min_valid_version) {
    H5Epush_error("Plugin version too old (security policy)");
    return FAIL;
}
```

**Advantages:**
- ✅ Enforces minimum version
- ✅ No footer format change needed
- ✅ Flexible policy updates

**Disadvantages:**
- ⚠️ Requires separate manifest file
- ⚠️ Organizations must distribute both files

**Status:** Possible future enhancement

#### Option B: Extend Footer Format (Breaking Change)

**Proposal for footer v2.0:**
```c
typedef struct H5PL_sig_footer_v2_t {
    uint64_t timestamp;         // Unix timestamp when signed
    uint32_t version_major;     // Plugin semantic version
    uint32_t version_minor;
    uint32_t version_patch;
    uint32_t signature_length;
    uint32_t magic;             // 0x48444636 (different for v2)
} H5PL_sig_footer_v2_t;
```

**Version policy:**
```c
// Organization maintains policy
typedef struct {
    char *plugin_name;
    uint32_t min_major;
    uint32_t min_minor;
    uint32_t min_patch;
} version_policy_t;

version_policy_t policies[] = {
    {"libh5zzfp.so", 1, 0, 1},  // Minimum v1.0.1
};

// Check at load time
if (plugin_version < policy_min_version) {
    H5Epush_error("Plugin version too old");
    return FAIL;
}
```

**Status:** Possible future enhancement (v2.0 footer format)

#### Option C: Trust Distribution Channels

**Current Best Practice:**

```bash
# Download from trusted sources
wget https://github.com/LLNL/H5Z-ZFP/releases/download/v1.0.1/libh5zzfp.so

# Verify checksum (published on release page)
sha256sum libh5zzfp.so
# Compare with: abc123def456... (from GitHub release notes)

# Verify signature (HDF5 does this automatically)
h5sign -v libh5zzfp.so -k llnl-public.pem
```

**Organizational policy:**
```bash
# Maintain approved versions list
# /etc/hdf5/approved-plugins.txt
libh5zzfp.so 1.0.1 sha256:abc123def456...
libh5blosc.so 2.3.0 sha256:xyz789ghi012...

# Verify before installation
if ! grep -q "libh5zzfp.so 1.0.1" /etc/hdf5/approved-plugins.txt; then
    echo "Plugin version not approved"
    exit 1
fi
```

### When This Becomes Critical

**Escalate if:**
- ✅ Rollback attack is demonstrated in the wild
- ✅ Multiple vulnerabilities discovered in plugins
- ✅ HTTPS/GitHub trust is compromised
- ✅ Organizations request explicit version enforcement

**Current assessment:** Low risk given trusted distribution channels

---

## Comprehensive Security Analysis

### Threat Matrix

| Threat | Raw RSA Protection | PKI Protection | Likelihood | Impact | Risk Level |
|--------|-------------------|----------------|------------|--------|------------|
| **Malicious unsigned plugin** | ✅ Prevents | ✅ Prevents | HIGH | HIGH | 🔴 CRITICAL → ✅ Mitigated |
| **Tampered signed plugin** | ✅ Detects | ✅ Detects | MEDIUM | HIGH | 🔴 CRITICAL → ✅ Mitigated |
| **Multi-vendor plugins** | ✅ KeyStore | ✅ PKI | MEDIUM | MEDIUM | 🟡 MEDIUM → ✅ Resolved |
| **Compromised developer key** | ⚠️ Slow response | ✅ Fast CRL | LOW | HIGH | 🟡 MEDIUM → ⚠️ Accepted |
| **Rollback to vulnerable version** | ❌ No protection | ⚠️ Partial | LOW | MEDIUM | 🟢 LOW → ⚠️ Accepted |
| **Supply chain attack** | ✅ Blocks | ✅ Blocks | LOW | HIGH | 🟡 MEDIUM → ✅ Mitigated |
| **Key sharing between orgs** | ⚠️ Possible | ⚠️ Possible | VERY LOW | HIGH | 🟢 LOW → Policy |

### Security Coverage

**What Raw RSA + KeyStore Protects Against:**
- ✅ Unsigned malicious plugins (primary threat)
- ✅ Modified legitimate plugins (tampering)
- ✅ Supply chain attacks (if keys protected)
- ✅ Multi-vendor trust (via KeyStore)
- ✅ Runtime trust changes (KeyStore updates)

**What Raw RSA Does NOT Protect Against:**
- ❌ Key compromise (no revocation)
- ❌ Rollback attacks (no version enforcement)
- ❌ Time-based attacks (no expiration)

**Comparison with PKI:**

| Feature | Raw RSA + KeyStore | Full PKI | Winner |
|---------|-------------------|----------|--------|
| Block malicious plugins | ✅ Yes | ✅ Yes | Tie |
| Detect tampering | ✅ Yes | ✅ Yes | Tie |
| Multi-vendor trust | ✅ Yes | ✅ Yes | Tie |
| Key revocation | ❌ Manual | ✅ CRL/OCSP | PKI |
| Certificate expiration | ❌ No | ✅ Yes | PKI |
| Rollback protection | ⚠️ Partial | ⚠️ Partial | Tie |
| Air-gapped support | ✅ Perfect | ⚠️ Difficult | RSA |
| Operational cost | ✅ $0 | ❌ $50K-120K/year | RSA |
| Staffing requirement | ✅ 0 FTE | ❌ 0.5-1 FTE | RSA |
| Implementation time | ✅ Done | ❌ 12+ months | RSA |

---

## Risk Assessment

### Current Risk Posture

**Overall Security Level:** ADEQUATE for HDF5's threat model

**Justification:**
1. **Primary threats mitigated:**
   - Malicious plugins blocked ✅
   - Tampering detected ✅
   - Multi-vendor trust supported ✅

2. **Secondary threats accepted:**
   - Key compromise (low likelihood, slow response acceptable)
   - Rollback attacks (low likelihood, trusted channels)

3. **Cost-benefit analysis:**
   - Zero operational cost vs $50K-120K/year (PKI)
   - Zero staffing vs 0.5-1 FTE (PKI)
   - Adequate security vs enterprise security

### Risk Acceptance Statement

**The HDF Group accepts the following risks:**

1. **No real-time revocation**
   - Risk: Compromised key can sign malicious plugins until manually removed
   - Mitigation: Key protection, monitoring, community reporting
   - Escalation: Move to PKI if incident occurs

2. **No rollback protection**
   - Risk: Attacker can serve old vulnerable versions
   - Mitigation: Trusted distribution channels, checksums, version policies
   - Escalation: Add version enforcement if attacks observed

3. **Manual trust updates**
   - Risk: Organizations must manually update KeyStore
   - Mitigation: Clear documentation, automation scripts
   - Escalation: None needed (acceptable for HDF5 ecosystem)

**Signed by:** [HDF Group Security Team]
**Date:** January 2026
**Review Date:** January 2027

---

## Escalation Triggers

### When to Immediately Reconsider PKI

**Critical triggers (act within 1 week):**
- 🚨 Developer private key compromised
- 🚨 Malicious signed plugin discovered in wild
- 🚨 Successful rollback attack demonstrated
- 🚨 Legal/insurance requires revocation capability

**High priority triggers (act within 1 month):**
- ⚠️ Multiple security incidents in plugin ecosystem
- ⚠️ Regulatory requirements mandate code signing
- ⚠️ Large organizations demand PKI for procurement

**Medium priority triggers (evaluate in 6-12 months):**
- 📊 Plugin ecosystem grows beyond 100 plugins
- 📊 Commercial plugin market emerges
- 📊 Dedicated funding secured ($100K+/year)

### Response Plan

**If critical trigger occurs:**

```
Hour 0:    Incident detected
           - Alert HDF Group security team
           - Notify affected organizations

Hour 1-4:  Immediate mitigation
           - Identify compromised key
           - Remove from published KeyStores
           - Alert community via mailing list, GitHub

Day 1-7:   Emergency response
           - Generate new key pair
           - Re-sign all legitimate plugins
           - Publish updated KeyStore configurations
           - Update documentation

Week 1-4:  PKI assessment
           - Convene security working group
           - Cost-benefit analysis for PKI
           - Funding/staffing evaluation
           - Go/no-go decision

Month 1-6: PKI implementation (if approved)
           - Procure HSM
           - Set up CA infrastructure
           - Migrate ecosystem
```

---

## Best Practices

### For Plugin Developers

**Key Management:**
```bash
# Generate key with strong passphrase
openssl genrsa -aes256 -out private.pem 2048

# Store in secure location
# - HSM (best)
# - Encrypted filesystem (good)
# - GitHub Secrets (acceptable for CI/CD)
# - Password manager (minimum)

# NEVER:
# - Commit to git
# - Store unencrypted
# - Share between developers
# - Use same key for multiple projects
```

**Signing Process:**
```bash
# Sign in secure environment
# Offline/air-gapped system preferred

h5sign -p plugin.so -k private.pem

# Verify before distribution
h5sign -v plugin.so -k public.pem
```

**Key Rotation:**
```bash
# Rotate every 1-2 years
# Or immediately if:
# - Developer leaves organization
# - System compromise suspected
# - Best practice refresh

# Process:
1. Generate new key pair
2. Sign new releases with new key
3. Keep old key for 6-12 months (transition)
4. Notify users of key change
5. Securely destroy old private key
```

### For Organizations/Users

**KeyStore Setup:**
```bash
# Create KeyStore directory
sudo mkdir -p /etc/hdf5/trusted_keys
sudo chmod 755 /etc/hdf5/trusted_keys

# Add trusted organization keys
sudo cp llnl_public.pem /etc/hdf5/trusted_keys/
sudo cp anl_public.pem /etc/hdf5/trusted_keys/
sudo cp hdfgroup_public.pem /etc/hdf5/trusted_keys/

# Verify permissions (NOT world-writable)
ls -ld /etc/hdf5/trusted_keys
# Should show: drwxr-xr-x (755)

# Build HDF5
cmake -DHDF5_REQUIRE_SIGNED_PLUGINS=ON \
      -DHDF5_PLUGIN_KEYSTORE_DIR=/etc/hdf5/trusted_keys
```

**Plugin Verification:**
```bash
# Download plugin
wget https://github.com/LLNL/H5Z-ZFP/releases/download/v1.0.1/libh5zzfp.so

# Verify checksum (from release notes)
sha256sum libh5zzfp.so
# Compare with published hash

# Verify signature (manual check)
h5sign -v libh5zzfp.so -k /etc/hdf5/trusted_keys/llnl_public.pem

# Install if both checks pass
sudo cp libh5zzfp.so /usr/local/hdf5/lib/plugin/
```

**Trust Management:**
```bash
# Review trusted keys periodically
ls -lh /etc/hdf5/trusted_keys/

# Add new trusted organization
sudo cp new_org_public.pem /etc/hdf5/trusted_keys/

# Remove compromised/untrusted key
sudo rm /etc/hdf5/trusted_keys/compromised_key.pem

# No recompilation needed! ✓
```

### For HDF Group

**Monitoring:**
- Monitor GitHub for suspicious plugin releases
- Subscribe to security mailing lists (CVEs)
- Track plugin ecosystem for anomalies

**Communication:**
- Maintain security mailing list
- Publish security advisories
- Document incident response procedures

**Documentation:**
- Keep best practices updated
- Provide example configurations
- Maintain plugin registry

---

## Conclusion

### Summary

The HDF5 plugin signature verification system provides **adequate security** for the ecosystem's threat model while maintaining **zero operational cost and staffing**.

**Key Points:**

1. ✅ **Primary threats mitigated** (malicious/tampered plugins)
2. ✅ **Multi-vendor trust resolved** (KeyStore implementation)
3. ⚠️ **Secondary risks accepted** (no revocation, no rollback protection)
4. ✅ **Cost-effective** ($0/year vs $50K-120K/year for PKI)
5. ✅ **Sustainable** (no ongoing staffing burden)

### Decision Rationale

**Why not PKI?**
- Small ecosystem (~20-30 plugins)
- Limited funding (no $50K-120K budget)
- Limited staffing (no 0.5-1 FTE available)
- No demonstrated incidents requiring revocation
- Air-gapped environments are common (CRL problematic)

**When to revisit:**
- If critical security incident occurs
- If ecosystem grows 5-10x
- If dedicated funding secured
- If regulatory requirements change

### Final Recommendation

**Continue with Raw RSA + KeyStore approach** with the following enhancements:

**Immediate (0-3 months):**
- ✅ Document known limitations (this document)
- ✅ Publish best practices for developers/users
- ✅ Establish incident response procedures

**Short-term (3-6 months):**
- 📋 Implement plugin registry (discovery, not enforcement)
- 📋 Consider adding blocklist support (optional revocation)
- 📋 Gather ecosystem feedback

**Long-term (1-2 years):**
- 🔄 Monitor for security incidents
- 🔄 Evaluate PKI if triggers occur
- 🔄 Consider footer v2.0 (timestamps/versions)

**The current architecture is fit for purpose and should not be changed without demonstrated need.**

---

## References

### Documentation
- Implementation: `src/H5PLsig.c`, `src/H5PLsig.h`
- KeyStore Design: `KEYSTORE_DESIGN.md`
- Testing Guide: `KEYSTORE_TESTING.md`
- Overall Plan: `PLUGIN_SIGNATURE_PLAN.md`

### Standards
- RSA PKCS#1 v2.2: https://tools.ietf.org/html/rfc8017
- SHA-256 (FIPS 180-4): https://csrc.nist.gov/publications/fips/fips180-4
- OpenSSL Documentation: https://www.openssl.org/docs/

### Contact
- Security issues: security@hdfgroup.org
- Technical questions: help@hdfgroup.org

---

*Document Version: 1.0*
*Last Updated: 2026-01-27*
*Next Review: 2027-01-27*
*Status: Active*
