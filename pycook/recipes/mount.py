def force_unmount_cifs(recipe):
    return "sudo umount -a -t cifs -l"
