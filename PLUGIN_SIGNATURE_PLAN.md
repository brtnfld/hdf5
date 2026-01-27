# HDF5 Plugin Signature Verification Plan

## Executive Summary

This document outlines the strategy for HDF5 plugin signature verification, balancing security requirements with operational constraints (limited funding and staffing).

**Decision:** Implement **raw RSA signature verification** with optional plugin registry, avoiding full PKI infrastructure due to operational costs ($50K-120K/year) and staffing requirements (0.5-1 FTE).

---

## Current Implementation Status

### ✅ Completed (January 2026)

1. **Raw RSA Signature Verification**
   - Location: `src/H5PLsig.c`, `src/H5PLsig.h`
   - Compile-time public key embedding
   - Runtime signature verification using OpenSSL
   - Appended signature format (plugin binary + signature + footer)

2. **Signing Tool**
   - Location: `tools/src/h5sign/h5sign.c`
   - C tool: `h5sign` (command-line utility)
   - Signs plugins with RSA private key using OpenSSL
   - Appends signature footer with magic number
   - Tests: `tools/test/h5sign/`

3. **Comprehensive Tests**
   - Location: `test/test_plugin_signature.c`
   - Test cases:
     - Valid signed plugin acceptance ✓
     - Unsigned plugin rejection ✓
     - Tampered plugin rejection ✓
     - Invalid signature rejection ✓
     - Missing footer rejection ✓
     - Corrupt magic number rejection ✓

4. **CI/CD Integration**
   - Workflow: `.github/workflows/signed-plugins.yml`
   - Automated testing on all commits
   - Cross-platform validation (Linux, macOS, Windows)

5. **KeyStore Implementation** ✨ NEW (January 2026)
   - Location: `src/H5PLsig.c`, `src/H5PLsig.h`
   - **Multiple trusted keys** from directory (LLNL, ANL, HDFGroup, etc.)
   - **Runtime key management** - add/remove keys without recompiling
   - **Three-tier key loading**:
     1. Environment variable: `HDF5_PLUGIN_KEYSTORE`
     2. CMake directory: `HDF5_PLUGIN_KEYSTORE_DIR`
     3. Embedded key fallback: `H5PL_PUBLIC_KEY_PEM` (backward compatibility)
   - **OR verification logic** - plugin succeeds if ANY key matches
   - **Security checks** - rejects world-writable directories
   - Design: `KEYSTORE_DESIGN.md`
   - Testing guide: `KEYSTORE_TESTING.md`

---

## Architecture

### Current Approach: Raw RSA with KeyStore

**KeyStore Approach (Recommended)** - Supports multiple trusted organizations:

```
┌─────────────────────────────────────────────────────────────┐
│  System Administrator                                        │
│                                                              │
│  1. Setup KeyStore directory:                               │
│     mkdir -p /etc/hdf5/trusted_keys                         │
│     chmod 755 /etc/hdf5/trusted_keys                        │
│                                                              │
│  2. Add trusted organization keys:                          │
│     cp llnl_public.pem /etc/hdf5/trusted_keys/              │
│     cp anl_public.pem /etc/hdf5/trusted_keys/               │
│     cp hdfgroup_public.pem /etc/hdf5/trusted_keys/          │
│                                                              │
│  3. Build HDF5 with KeyStore:                               │
│     cmake -DHDF5_REQUIRE_SIGNED_PLUGINS=ON \                │
│           -DHDF5_PLUGIN_KEYSTORE_DIR=/etc/hdf5/trusted_keys │
│                                                              │
│  4. Plugins from ANY trusted org are accepted:              │
│     - LLNL plugins ✓                                        │
│     - ANL plugins ✓                                         │
│     - HDFGroup plugins ✓                                    │
│                                                              │
│  5. Add/remove trust without recompiling:                   │
│     cp ornl_public.pem /etc/hdf5/trusted_keys/  # Add ORNL │
│     rm /etc/hdf5/trusted_keys/llnl_public.pem   # Remove   │
└─────────────────────────────────────────────────────────────┘
```

**Legacy Single-Key Approach (Backward Compatible)**:

```
┌─────────────────────────────────────────────────────────────┐
│  Organization (e.g., LLNL, ORNL)                            │
│                                                              │
│  1. Generate key pair:                                      │
│     openssl genrsa -out private.pem 2048                    │
│     openssl rsa -in private.pem -pubout -out public.pem     │
│                                                              │
│  2. Compile HDF5 with org's public key:                     │
│     cmake -DHDF5_REQUIRE_SIGNED_PLUGINS=ON \                │
│           -DHDF5_PLUGIN_PUBLIC_KEY_FILE=public.pem          │
│                                                              │
│  3. Sign plugins with org's private key:                    │
│     h5sign -p plugin.so -k private.pem                       │
│                                                              │
│  4. Distribute:                                             │
│     - Signed plugins to users                               │
│     - HDF5 library (with embedded public key)               │
└─────────────────────────────────────────────────────────────┘
```

### Trust Model

**Direct Key Trust** (not hierarchical)
- Each organization manages their own key pair
- Organizations control which plugins they trust
- No central authority required
- Perfect for high-security environments (DoE labs, air-gapped systems)

### File Format

```
┌──────────────────────────────────────┐
│ Plugin Binary (.so/.dll)             │
│ (Original compiled plugin)           │
├──────────────────────────────────────┤
│ RSA Signature (256 bytes for 2048-bit)│
├──────────────────────────────────────┤
│ Footer (8 bytes):                    │
│  - signature_length (4 bytes, LE)    │
│  - magic number (4 bytes, LE)        │
│    0x48444635 ("HDF5")               │
└──────────────────────────────────────┘
```

**Note:** Little-endian encoding for cross-platform compatibility.

---

## Phased Implementation Plan

### Phase 1: Documentation & Announcement (Weeks 1-2)

**Goal:** Make current feature usable and discoverable

**Tasks:**
- [ ] Document raw RSA approach in main README
- [ ] Create detailed security documentation
- [ ] Write developer guide (how to sign plugins)
- [ ] Write user guide (how to enable verification)
- [ ] Announce feature on HDF5 mailing list
- [ ] Blog post explaining security model

**Deliverables:**
- `docs/PLUGIN_SECURITY.md` - Comprehensive security guide
- `docs/PLUGIN_SIGNING_GUIDE.md` - Developer instructions
- Updated `README.md` with security section

**Effort:** 1-2 weeks, 1 person
**Cost:** $0

---

### Phase 2: Plugin Registry (Months 2-4)

**Goal:** Create curated list of known plugins (optional, non-enforcement)

#### Implementation

**Registry Repository:** `https://github.com/HDFGroup/hdf5-plugin-registry`

**Registry Format (YAML):**
```yaml
# registry.yaml
version: "1.0"
last_updated: "2026-04-15"

plugins:
  - name: H5Z-ZFP
    description: "ZFP compression filter"
    developer: Lawrence Livermore National Laboratory
    homepage: https://github.com/LLNL/H5Z-ZFP
    contact: zfp-dev@llnl.gov
    verification:
      method: pgp
      key_fingerprint: "ABCD 1234 5678 90EF GHIJ KLMN OPQR STUV WXYZ"
      key_url: https://github.com/LLNL/H5Z-ZFP/blob/main/SIGNING_KEY.pub
    status: verified_partner
    last_verified: "2026-01-15"

  - name: HDF5-Blosc
    description: "Blosc compression filter"
    developer: Blosc Development Team
    homepage: https://github.com/Blosc/hdf5-blosc
    contact: blosc@blosc.org
    verification:
      method: rsa
      key_url: https://github.com/Blosc/hdf5-blosc/blob/main/pubkey.pem
    status: community
    last_verified: "2026-02-01"
```

#### Workflow

```
Plugin Developer:
1. Fork repository
2. Add plugin entry to registry.yaml
3. Submit pull request
4. Provide proof of identity (email, GitHub ownership, etc.)

HDF Group Staff:
1. Review PR (verify developer is legitimate)
2. Check plugin quality/security (basic review)
3. Merge PR (takes 1-2 days)

Users:
1. Browse registry on website
2. Find trusted plugins
3. Download and verify independently
```

#### Optional Library Integration

```c
// Optional registry checking (warnings, not enforcement)
#ifdef HDF5_ENABLE_PLUGIN_REGISTRY
    H5PL_registry_status_t status = H5PL_check_registry(plugin_path);

    if (status == H5PL_REGISTRY_VERIFIED) {
        printf("✓ Plugin verified in HDF Group registry\n");
    }
    else if (status == H5PL_REGISTRY_UNKNOWN) {
        printf("⚠ Plugin not in HDF Group registry (not necessarily unsafe)\n");
    }
#endif

// Then proceed with normal signature verification
herr_t result = H5PL__verify_signature_appended(plugin_path);
```

**Deliverables:**
- GitHub repository with registry.yaml
- Web interface (GitHub Pages)
- Developer submission guide
- Optional library support (compile-time flag)

**Effort:** 2-4 weeks total work (spread over 3 months)
**Staffing:** 1 person, 1-2 hours/week for PR review
**Cost:** $0 (GitHub Pages hosting)

---

### Phase 3: Ecosystem Development (Months 4-12)

**Goal:** Build adoption and gather feedback

**Tasks:**
- [ ] Contact major plugin developers (LLNL, ANL, ORNL, Blosc)
- [ ] Get 5-10 plugins registered
- [ ] Gather developer feedback
- [ ] Iterate on documentation
- [ ] Monitor usage patterns
- [ ] Identify pain points

**Success Metrics:**
- 10+ plugins in registry
- 3+ organizations using signed plugins
- Positive developer feedback
- Zero security incidents

**Effort:** Ongoing, low intensity
**Staffing:** 1 person, 2-4 hours/month
**Cost:** $0

---

### Phase 4: Future Evaluation (Year 2-3)

**Goal:** Decide if advanced features are needed

**Evaluation Criteria:**

| Metric | Current State | Threshold for PKI |
|--------|--------------|-------------------|
| Plugin count | ~20-30 | >100 |
| Ecosystem size | Small | Large commercial market |
| Developer demand | Low | High demand for central CA |
| Security incidents | None expected | Multiple incidents |
| HDF Group budget | Limited | $100K+/year available |
| Staffing | Limited | 0.5-1 FTE available |

**Reassess if:**
1. Ecosystem grows significantly (>100 plugins)
2. Commercial plugin market emerges
3. Regulatory requirements mandate code signing
4. Multiple security incidents occur
5. Dedicated funding secured

**Options to Consider (in priority order):**

1. **Sigstore/Cosign Integration** (Modern, low-cost)
   - Uses GitHub OIDC for identity
   - Transparency log for audit
   - Zero infrastructure cost
   - Automatic with CI/CD
   - **Cost:** $0, **Effort:** 4-6 weeks

2. **PGP Web of Trust** (Traditional, zero-cost)
   - HDF Group signs developer GPG keys
   - Developers sign plugins with GPG
   - Leverages existing infrastructure
   - **Cost:** $0, **Effort:** Documentation only

3. **Full PKI** (Enterprise-grade, high-cost)
   - Only if justified by ecosystem size
   - Requires dedicated funding
   - **Cost:** $50K-120K/year, **Effort:** Full-time staff

---

## Alternative Approaches (Ranked by Best Fit for Constraints)

**Evaluation Criteria:**
- ✅ Limited funding (prefer $0 cost)
- ✅ Limited staffing (prefer 0 FTE)
- Security effectiveness
- Sustainability (can maintain long-term)
- Fits HDF5 use cases (air-gapped, HPC, etc.)

---

### 🥇 #1: Raw RSA (Current Implementation) - **RECOMMENDED**

**Status:** ✅ Implemented and tested

**Cost:** $0/year
**Staffing:** 0 FTE
**Implementation Time:** ✅ Done

#### How It Works:
```bash
# Organizations manage their own keys
openssl genrsa -out org-private.pem 2048
openssl rsa -in org-private.pem -pubout -out org-public.pem

# Compile HDF5 with organization's public key
cmake -DH5PL_PUBLIC_KEY_PEM="$(cat org-public.pem)"

# Sign plugins with organization's private key
h5sign -p plugin.so -k org-private.pem
```

#### Advantages:
- ✅ **Zero cost** - No infrastructure needed
- ✅ **Zero staffing** - Each org manages own keys
- ✅ **Already complete** - Tests passing, CI integrated
- ✅ **Perfect for air-gapped** - No internet required
- ✅ **Flexible** - Each org controls their own trust
- ✅ **Sustainable** - No ongoing maintenance burden
- ✅ **Proven approach** - Used by Linux kernel modules

#### Disadvantages:
- No central "HDF Group blessed" plugins
- Organizations must manage own keys
- No revocation mechanism (but keys can be rotated)

#### Best For:
- High-security environments (DoE labs, defense)
- Air-gapped HPC systems
- Organizations with specific security policies
- Small plugin ecosystems

#### **Verdict: Perfect match for HDF Group's constraints**

---

### 🥈 #2: Public Plugin Registry - **RECOMMENDED ADDITION**

**Status:** Not yet implemented

**Cost:** $0/year (GitHub Pages)
**Staffing:** 0.1 FTE (1-2 hours/week)
**Implementation Time:** 2-4 weeks

#### How It Works:
```yaml
# GitHub repo: HDFGroup/hdf5-plugin-registry
# File: registry.yaml

plugins:
  - name: H5Z-ZFP
    developer: LLNL
    key_url: https://github.com/LLNL/H5Z-ZFP/blob/main/pubkey.pem
    status: verified_partner
```

#### Advantages:
- ✅ **Zero infrastructure cost** - Just a GitHub repo
- ✅ **Minimal staffing** - Simple PR reviews
- ✅ **Complements raw RSA** - Doesn't replace it
- ✅ **Adds discovery** - Users find trusted plugins
- ✅ **Non-enforcement** - Informational only
- ✅ **Easy to maintain** - Simple YAML file

#### Disadvantages:
- Requires occasional staff time for PR reviews
- Not a security mechanism (just discovery)

#### Best For:
- Plugin discovery
- Trust signaling ("HDF Group knows about this")
- Complementing raw RSA verification

#### **Verdict: Low-effort value add to current approach**

---

### 🥉 #3: PGP Web of Trust - **ALTERNATIVE**

**Status:** Not implemented

**Cost:** $0/year
**Staffing:** 0 FTE
**Implementation Time:** Documentation only (1-2 weeks)

#### How It Works:
```bash
# HDF Group staff member signs plugin developer's GPG key
gpg --sign-key developer@llnl.gov

# Developer signs plugins with GPG
gpg --detach-sign plugin.so

# Users trust HDF Group's key, transitively trust signed developers
```

#### Advantages:
- ✅ **Zero cost** - Uses existing PGP infrastructure
- ✅ **Zero staffing** - No ongoing operations
- ✅ **Hierarchical trust** - HDF Group vouches for developers
- ✅ **Works offline** - After initial key distribution
- ✅ **Standard approach** - Used by Linux distros (apt/yum)

#### Disadvantages:
- Complex for non-technical users
- PGP has usability issues
- Requires GPG tooling on all platforms
- Learning curve for developers

#### Best For:
- Technical communities
- Linux-heavy environments
- When hierarchical trust is needed

#### **Verdict: Good alternative but more complex than raw RSA**

---

### 4️⃣ #4: Sigstore/Cosign (Modern Approach) - **FUTURE CONSIDERATION**

**Status:** Not implemented

**Cost:** $0/year (uses sigstore.dev infrastructure)
**Staffing:** 0.1 FTE
**Implementation Time:** 4-6 weeks

#### How It Works:
```yaml
# GitHub Actions automatically signs on release
# Uses GitHub OIDC identity, no keys needed
# Transparency log provides audit trail

- uses: sigstore/cosign-action@v2
  with:
    repository: LLNL/H5Z-ZFP
```

#### Advantages:
- ✅ **Zero infrastructure** - Uses public sigstore
- ✅ **No key management** - OIDC-based identity
- ✅ **Transparency log** - Full audit trail
- ✅ **Modern approach** - Growing adoption (npm, PyPI)
- ✅ **Automatic** - Integrates with CI/CD

#### Disadvantages:
- ❌ **Requires internet** - Can't verify offline (deal-breaker for air-gapped)
- ❌ **New technology** - Less proven than traditional approaches
- ❌ **Depends on GitHub** - Single point of trust
- Adds external dependency (sigstore infrastructure)

#### Best For:
- Cloud-native environments
- CI/CD-heavy workflows
- When internet connectivity is guaranteed
- Future-looking implementations

#### **Verdict: Revisit in 2-3 years when technology matures and air-gapped story improves**

---

### ❌ #5: Full PKI (Certificate Authority) - **NOT RECOMMENDED**

**Status:** Not implemented

**Cost:** $50K-120K/year
**Staffing:** 0.5-1 FTE
**Implementation Time:** 12+ months

#### What It Requires:
```
Infrastructure:
- HSM (Hardware Security Module): $15K-50K upfront
- Secure servers: $500-2K/month
- Backup/DR: $1K-5K/year
- Insurance: $5K-20K/year

Staffing:
- Certificate review and approval
- Key ceremony procedures
- 24/7 incident response
- Annual compliance audits
- Revocation management

Operations:
- Root CA key ceremony
- Intermediate CA management
- Certificate issuance workflow
- CRL distribution
- Legal/compliance documentation
```

#### Advantages:
- Industry-standard approach
- Hierarchical trust model
- Certificate revocation support
- Professional operations

#### Disadvantages:
- ❌ **High cost** - Not sustainable with limited budget
- ❌ **High staffing** - Requires dedicated personnel
- ❌ **Complex operations** - HSM, key ceremonies, audits
- ❌ **Long timeline** - 12+ months to implement
- ❌ **Overkill** - Only ~20-30 plugins in ecosystem
- ❌ **Air-gapped issues** - CRL distribution challenges

#### Best For:
- Large ecosystems (1000+ plugins)
- Commercial software markets
- When dedicated funding available
- Regulatory compliance requirements

#### **Verdict: Does not match HDF Group's constraints. Revisit only if ecosystem grows 5-10x AND dedicated funding secured**

---

### 6️⃣ #6: Commercial Code Signing - **NOT RECOMMENDED**

**Status:** Not implemented

**Cost:** $200-500/year per developer
**Staffing:** 0 FTE (outsourced)
**Implementation Time:** 2-4 weeks

#### How It Works:
```bash
# Developers purchase code signing cert from DigiCert/Sectigo
# Sign plugins with commercial certificate
# HDF5 validates against OS trust store
```

#### Advantages:
- No HDF Group infrastructure
- Established industry practice
- Revocation support (OCSP/CRL)

#### Disadvantages:
- ❌ **Cost to developers** - Barrier to entry
- ❌ **Annual renewal** - Ongoing expense
- ❌ **Not flexible** - Can't customize trust model
- ❌ **Poor for air-gapped** - Requires internet for OCSP
- ❌ **Rigid policies** - Can't adapt to HDF5 needs

#### Best For:
- Commercial software distribution
- Windows applications
- When developers already have code signing certs

#### **Verdict: Too rigid, doesn't match HDF5 ecosystem needs**

---

## Recommendation Summary

### Immediate Action (Now):
✅ **Use Raw RSA** (#1) - Already implemented, perfect fit

### Short-term Enhancement (3-6 months):
✅ **Add Plugin Registry** (#2) - Low effort, adds value

### Not Recommended:
❌ **Full PKI** (#5) - Doesn't match constraints
❌ **Commercial Code Signing** (#6) - Too rigid

### Reevaluate Later (2-3 years):
🔄 **Sigstore** (#4) - When technology matures
🔄 **PGP Web of Trust** (#3) - If community requests

---

## Decision Matrix

| Approach | Annual Cost | Staffing | Air-gapped | Status | Recommendation |
|----------|-------------|----------|------------|--------|----------------|
| **Raw RSA (current)** | $0 | 0 FTE | ✅ Perfect | ✅ Done | ⭐⭐⭐ Use now |
| **Plugin Registry** | $0 | 0.1 FTE | ✅ Works | Not done | ⭐⭐⭐ Add next |
| **PGP Web of Trust** | $0 | 0 FTE | ✅ Perfect | Not done | ⭐⭐ Alternative |
| **Sigstore** | $0 | 0.1 FTE | ❌ No | Not done | ⭐ Future only |
| **Full PKI** | $50K-120K | 0.5-1 FTE | ⚠️ Difficult | Not done | ❌ Not viable |
| **Commercial** | Dev cost | 0 FTE | ❌ No | Not done | ❌ Not suitable |

**Key:**
- ⭐⭐⭐ = Highly recommended
- ⭐⭐ = Good alternative
- ⭐ = Consider for future
- ❌ = Not recommended

---

## Technical Specifications

### Cryptographic Requirements

**Algorithm:** RSA with SHA-256 (current standard)

**Key Sizes:**
- **Minimum:** 2048-bit RSA (current)
- **Recommended:** 3072-bit RSA (future-proof)
- **Maximum:** 4096-bit RSA (slower, rarely needed)

**Alternative:** ECDSA P-256 (smaller signatures, faster)

### Signature Format Specification

**Version 1.0 (Current):**

```c
typedef struct H5PL_sig_footer_t {
    uint32_t signature_length;  /* RSA signature length (256 bytes for 2048-bit) */
    uint32_t magic;             /* Magic number: 0x48444635 ("HDF5") */
} H5PL_sig_footer_t;

/* Both fields encoded in little-endian on disk */
/* Total footer size: 8 bytes */
```

**File Layout:**
```
[Plugin Binary] [Signature] [Footer]
               ↑            ↑
               |            +-- 8 bytes (fixed)
               +-- Variable (256 bytes for RSA-2048)
```

**Verification Process:**
1. Seek to end of file - 8 bytes
2. Read footer
3. Decode footer (little-endian → native)
4. Verify magic number == 0x48444635
5. Seek to end - 8 - signature_length
6. Read signature bytes
7. Compute SHA-256 hash of [0 : end - 8 - signature_length]
8. Verify signature using OpenSSL EVP_DigestVerify*()

### OpenSSL Integration

**Minimum Version:** OpenSSL 1.1.1 or later
**Recommended:** OpenSSL 3.0+

**API Usage:**
- `EVP_PKEY` for key management
- `EVP_DigestVerify*()` for signature verification
- Avoids deprecated `RSA_*` functions

**Build Configuration:**
```cmake
if(HDF5_REQUIRE_SIGNED_PLUGINS)
    find_package(OpenSSL 1.1.1 REQUIRED)
    target_link_libraries(hdf5 PRIVATE OpenSSL::Crypto)
endif()
```

---

## Security Considerations

### Threat Model

**What This Protects Against:**
- ✅ Malicious plugins (unsigned plugins rejected)
- ✅ Tampered plugins (signature mismatch)
- ✅ Supply chain attacks (only signed plugins loaded)

**What This Does NOT Protect Against:**
- ❌ Compromised private keys (organization's responsibility)
- ❌ Vulnerabilities in signed plugins (code review needed)
- ❌ Social engineering (tricking users to install wrong key)

### Best Practices for Organizations

**Key Management:**
```bash
# Generate key on offline/secure system
openssl genrsa -aes256 -out private.pem 2048

# Encrypt private key with strong passphrase
# Store in:
# - Hardware Security Module (HSM) - Best
# - Encrypted filesystem - Good
# - Password-protected file - Minimum

# Backup private key to secure offline storage
# (fireproof safe, bank deposit box, etc.)

# Distribute public key freely
# - Embed in HDF5 builds
# - Publish on website
# - Include in documentation
```

**Signing Process:**
```bash
# Sign plugins in isolated environment
# Keep private key offline
# Use air-gapped system for high-security

# Automate signing in CI/CD using secrets management:
# - GitHub Secrets
# - HashiCorp Vault
# - AWS Secrets Manager
```

**Key Rotation:**
```bash
# Recommended rotation schedule:
# - Every 2 years for routine rotation
# - Immediately if compromise suspected

# Process:
# 1. Generate new key pair
# 2. Sign new plugins with new key
# 3. Re-sign critical old plugins
# 4. Distribute new public key
# 5. Securely destroy old private key (after transition period)
```

### Air-Gapped Environments

**This implementation is PERFECT for air-gapped systems:**

```
Advantages:
✓ No internet required for verification
✓ No CRL/OCSP checks needed
✓ No certificate expiration issues
✓ Complete control over trust
✓ Common in HPC/DoE environments

Deployment:
1. Build HDF5 with public key on secure system
2. Transfer compiled library to air-gapped network
3. Sign plugins on secure system
4. Transfer signed plugins to air-gapped network
5. Verification works completely offline
```

---

## Comparison with Alternatives

### Why Not Full PKI?

| Aspect | Raw RSA (Current) | Full PKI |
|--------|-------------------|----------|
| **Annual Cost** | $0 | $50K-120K |
| **Staffing** | 0 FTE | 0.5-1 FTE |
| **Setup Time** | ✅ Done | 12+ months |
| **Infrastructure** | None | HSM, servers, backups |
| **Complexity** | Low | High |
| **Air-gapped Support** | Perfect | Difficult (CRL updates) |
| **Revocation** | Not possible | CRL/OCSP |
| **Expiration** | Keys don't expire | Certificates expire |
| **Flexibility** | High (each org controls) | Low (central authority) |

**Decision:** PKI overhead not justified for small ecosystem.

### Why Not Commercial Code Signing?

**Option:** Require developers to use DigiCert/Sectigo code signing certificates

**Pros:**
- No HDF Group infrastructure
- Industry-standard process
- Revocation support

**Cons:**
- Cost to developers ($200-500/year per certificate)
- Annual renewal burden
- Not flexible (can't use custom trust)
- Poor for air-gapped environments
- Requires internet for validation

**Decision:** Too rigid, doesn't match HDF5 use cases.

### Why Not Sigstore/Cosign?

**Modern approach:** Uses transparency logs and OIDC

**Pros:**
- Free, zero infrastructure
- Automatic with GitHub Actions
- Transparency log (audit trail)
- Modern cryptography

**Cons:**
- Requires internet to verify (deal-breaker for air-gapped)
- Newer technology (less proven)
- Ties trust to GitHub/Google
- Not mature enough yet

**Decision:** Revisit in 2-3 years when technology matures.

---

## Developer Guide Summary

### For Plugin Developers

**Quick Start:**
```bash
# 1. Generate key pair (once)
openssl genrsa -out private.pem 2048
openssl rsa -in private.pem -pubout -out public.pem

# 2. Build your plugin
make plugin.so

# 3. Sign your plugin
h5sign -p plugin.so -k private.pem

# 4. Distribute
# - Signed plugin.so
# - Public key (public.pem)
# - Instructions for users
```

**Best Practices:**
- Keep private key secure and offline
- Sign all plugin releases
- Publish public key on your website/GitHub
- Document verification process for users
- Consider adding to HDF5 plugin registry

---

## User Guide Summary

### For HDF5 Users

**Enable Signature Verification:**
```bash
# 1. Obtain plugin developer's public key
wget https://example.org/plugins/public.pem

# 2. Build HDF5 with signature verification
cmake -B build \
  -DHDF5_REQUIRE_SIGNED_PLUGINS:BOOL=ON \
  -DH5PL_PUBLIC_KEY_PEM="$(cat public.pem)" \
  -DBUILD_SHARED_LIBS:BOOL=ON

cmake --build build

# 3. Install
cmake --install build

# 4. Use normally - only signed plugins will load
./your_application
# Unsigned plugins → ERROR: "Plugin signature verification failed"
```

**Security Levels:**
```bash
# OFF: No verification (default, backward compatible)
cmake -DHDF5_REQUIRE_SIGNED_PLUGINS:BOOL=OFF

# ON: Require valid signatures
cmake -DHDF5_REQUIRE_SIGNED_PLUGINS:BOOL=ON \
      -DH5PL_PUBLIC_KEY_PEM="$(cat public.pem)"
```

---

## Maintenance Plan

### Ongoing Responsibilities

**HDF Group:**
- Maintain test suite
- Review plugin registry PRs (1-2 hours/week)
- Respond to security issues
- Update documentation
- Monitor ecosystem

**Plugin Developers:**
- Sign their own plugins
- Manage their own keys
- Provide public keys to users
- Register plugins (optional)

**Users/Organizations:**
- Decide which plugins to trust
- Manage public key distribution
- Configure HDF5 builds

### Success Metrics

**Year 1:**
- [ ] 10+ plugins in registry
- [ ] 3+ organizations using signed plugins
- [ ] Zero security incidents
- [ ] Documentation complete

**Year 2-3:**
- [ ] 20+ plugins in registry
- [ ] 10+ organizations using signed plugins
- [ ] Community feedback positive
- [ ] Feature considered stable

---

## Decision Points

### When to Revisit PKI

**Trigger conditions:**
1. Plugin ecosystem exceeds 100 plugins
2. Commercial plugin market emerges
3. Dedicated funding secured ($100K+/year)
4. Multiple security incidents
5. Regulatory/compliance requirements
6. Community strongly requests it

**Process:**
1. Survey plugin developers
2. Cost-benefit analysis
3. Pilot program with 3-5 plugins
4. Measure adoption
5. Go/no-go decision

### When to Implement Revocation

**Current approach:** Keys don't expire, no revocation

**Alternatives if revocation needed:**
1. **Short-lived keys** (rotate every 90 days) - Simple
2. **CRL files** (manual distribution) - Traditional
3. **Registry-based** (blocklist in registry.yaml) - Lightweight
4. **Full PKI** (standard CRL/OCSP) - Complex

**Decision:** Implement only if security incident occurs.

---

## Conclusion

**Current Status:** ✅ Prototype complete and tested

**Recommended Approach:**
1. ✅ Raw RSA signature verification (implemented)
2. 🔄 Plugin registry (2-4 months)
3. ⏸️ Full PKI (not recommended, revisit in 2-3 years)

**Rationale:**
- Matches resource constraints (limited funding/staffing)
- Provides meaningful security improvements
- Flexible for diverse use cases (air-gapped, HPC, cloud)
- Zero ongoing operational burden
- Can evolve as ecosystem grows

**Next Steps:**
1. Complete documentation (Phase 1)
2. Set up plugin registry (Phase 2)
3. Engage with plugin developers
4. Monitor adoption and iterate

---

## References

### Documentation
- Plugin Security Guide: `docs/PLUGIN_SECURITY.md` (TBD)
- Developer Guide: `docs/PLUGIN_SIGNING_GUIDE.md` (TBD)
- Verification Implementation: `src/H5PLsig.c`, `src/H5PLsig.h`
- Signing Tool: `tools/src/h5sign/h5sign.c`
- Verification Tests: `test/test_plugin_signature.c`
- Signing Tool Tests: `tools/test/h5sign/`

### Related Standards
- RSA PKCS#1: https://tools.ietf.org/html/rfc8017
- SHA-256: FIPS 180-4
- OpenSSL: https://www.openssl.org/docs/

### Contact
- Security issues: security@hdfgroup.org
- Plugin registry: plugins@hdfgroup.org
- General questions: help@hdfgroup.org

---

*Last updated: 2026-01-26*
*Version: 1.0*
*Status: Active*
