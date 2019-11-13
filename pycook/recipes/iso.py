#* Imports
import pycook.elisp as el

#* Recipes
def burn_to_usb(recipe, fname):
    disks = el.sc_l("df")
    media_disks = el.re_filter("/media/", disks)
    assert len(media_disks) == 1
    usb_disk_data = media_disks[0].split()
    assert len(usb_disk_data) == 6
    usb_disk = usb_disk_data[0]
    fname_full = el.expand_file_name(fname)
    return [
        "sudo umount " + usb_disk,
        el.lf("sudo dd bs=4M if={fname_full} of={usb_disk} status=progress oflag=sync")]
