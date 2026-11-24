---
id: security.credentials
revision: 1
owner: wyo-security
strength: must
scope: [all]
final: true
kind: instruction
title: Never expose credentials or private data
enforcement:
  class: prohibition
  controls: [filesystem, permissions]
actions: [fs.read, net.fetch]
guards:
  - action: fs.read
    paths:
      - "**/.env"
      - "**/.env.*"
      - "**/*.pem"
      - "**/*.key"
      - "**/id_rsa*"
      - "**/id_ed25519*"
      - "**/.netrc"
      - "**/.aws/credentials"
  - action: net.fetch
    hosts: ["*"]
key: credentials.exposure
value: prohibited
---
Never read, print, commit, log, or transmit credentials or private data outside
the granted task and destination. Use environment-variable names or external
file references; never place a secret value in policy source, a rendered
artifact, a prompt, a manifest, or a test fixture.

A host grant that makes unassigned credentials unavailable is enforcement. A
sentence in a prompt is guidance. Do not claim the prohibition is enforced when
the selected adapter has no filesystem or permission control.
