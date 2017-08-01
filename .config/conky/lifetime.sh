# Lifetime ~ 75 years.
seconds=$(( $(date --date="640925" +%s) - $(date +%s) ))
eval "echo $(date -ud "@$seconds" +'$((%s/3600/24)) days %H hours %M minutes %S seconds')"
