{ ... }:
{
  boot = {
    loader = {
      timeout = 5;
      grub = {
        enable = true;
        gfxmodeEfi = "1920x1080";
        gfxmodeBios = "1920x1080";
        configurationLimit = 5;
      };
    };

    kernel.sysctl = {
      "kernel.nmi_watchdog" = 0;
      "vm.dirty_writeback_centisecs" = 6000;
    };

    initrd.kernelModules = [
      "ahci"
      "nvme"
      "sd_mod"
      "usb_storage"
      "usbhid"
      "xhci_pci"
    ];

    extraModprobeConfig = ''
      options zfs spa_slop_shift=6
    '';
  };
}
