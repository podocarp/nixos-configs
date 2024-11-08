{ ... }:
{
  boot = {
    loader = {
      timeout = 5;
      grub = {
        enable = true;
        gfxmodeEfi = "1366x768"; # speeds up draw time
        gfxmodeBios = "1366x768";
        configurationLimit = 5;
      };
    };

    kernel.sysctl = {
      "kernel.nmi_watchdog" = 0;
      "vm.dirty_writeback_centisecs" = 1500;
    };

    initrd.availableKernelModules = [
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
