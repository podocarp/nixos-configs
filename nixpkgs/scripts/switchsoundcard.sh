cat /proc/asound/cards
read -p "Select card number: " n

cat << EOF | sudo tee /etc/asound.conf
pcm.!default {
    type hw
    card $n
}
ctl.!default {
    type hw
    card $n
}
EOF
