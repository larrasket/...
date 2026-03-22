#!/usr/bin/env sh
# Extract SMTP password from authinfo.gpg for msmtp.
# gpg-agent caches the passphrase so this is non-interactive.
gpg -d --quiet ~/.authinfo.gpg 2>/dev/null | \
  awk '/machine smtp\.mail\.me\.com/ { for(i=1;i<=NF;i++) if($i=="password") print $(i+1) }'
