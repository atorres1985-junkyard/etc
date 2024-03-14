# # -*- mode:sh ; coding: utf-8-unix; fill-column: 80 -*-

udisks='auto'
colourize='1'
pretty='1'

# Set default mount options. The default is to pass no options.
mount_options='--options nosuid,noexec,noatime'

# Set devices to exclude. Any device whose "lsblk -P" output contains a string
# listed here will be hidden. The following key-value-pairs are printed:
# lsblk -dPno NAME,TYPE,FSTYPE,LABEL,MOUNTPOINT,PARTLABEL,UUID [device_name]
# The strings are matched using "grep -E" regular expression.
exclude=()

exclude+=( 'LABEL="SWAP"' )
exclude+=( 'MOUNTPOINT="/nix/store"' )
exclude+=( 'MOUNTPOINT="/root"' )
exclude+=( 'MOUNTPOINT="/boot"' )
exclude+=( 'MOUNTPOINT="/home"' )
exclude+=( 'MOUNTPOINT="/"' )

filemanager() {
   mc "$1"
}
