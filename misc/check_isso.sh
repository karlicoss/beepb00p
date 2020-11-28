#!/usr/bin/env bash
set -euxo pipefail

# ID=$(http --check-status post 'https://beepb00p.xyz/comments/new?uri=isso_org-sandbox' 'text="test2"' | jq .id)
# [[ -n "$ID" ]]
# 2>&1 echo "posted: $ID"

http --ignore-stdin --check-status post 'https://beepb00p.xyz/comments/new?uri=isso_org-sandbox' 'text="test"' 'author=karlicoss'
http --ignore-stdin --check-status get  'https://beepb00p.xyz/comments/?uri=isso_org-sandbox'

# eh, wouldn't be able to delete I suppose..
# http delete "https://beepb00p.xyz/comments/id/$ID"
