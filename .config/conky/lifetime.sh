# Lifetime ~ 75 years.
seconds=$(( $(date --date="640925" +%s) - $(date +%s) ))
eval "echo $(date -ud "@$seconds" +'$((%s/31556952)) years $((%s/86460%365)) days')"
