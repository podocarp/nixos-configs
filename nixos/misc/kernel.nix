{ pkgs, lib, ... }:
{
  boot.kernelPackages = pkgs.linuxCustom;
  nixpkgs.overlays = [
    (self: super: {
     linuxCustom = pkgs.linuxPackagesFor (pkgs.linux.override {
         structuredExtraConfig = with lib.kernel; {
         MACINTOSH_DRIVERS = no;
         NET_VENDOR_3COM = no;
         NET_VENDOR_ADAPTEC = no;
         NET_VENDOR_AGERE = no;
         NET_VENDOR_ALACRITECH = no;
         NET_VENDOR_ALTEON = no;
         NET_VENDOR_AMAZON = no;
         NET_VENDOR_AMD = no;
         NET_VENDOR_AQUANTIA = no;
         NET_VENDOR_ARC = no;
         NET_VENDOR_ATHEROS = no;
         NET_VENDOR_AURORA = no;
         NET_VENDOR_BROADCOM = no;
         NET_VENDOR_BROCADE = no;
         NET_VENDOR_CADENCE = no;
         NET_VENDOR_CAVIUM = no;
         NET_VENDOR_CHELSIO = no;
         NET_VENDOR_CISCO = no;
         NET_VENDOR_CORTINA = no;
         NET_VENDOR_DEC = no;
         NET_VENDOR_DLINK = no;
         NET_VENDOR_EMULEX = no;
         NET_VENDOR_EZCHIP = no;
         NET_VENDOR_FUJITSU = no;
         NET_VENDOR_GOOGLE = no;
         NET_VENDOR_HUAWEI = no;
         NET_VENDOR_MARVELL = no;
         NET_VENDOR_MELLANOX = no;
         NET_VENDOR_MICREL = no;
         NET_VENDOR_MICROCHIP = no;
         NET_VENDOR_MICROSEMI = no;
         NET_VENDOR_MYRI = no;
         NET_VENDOR_NATSEMI = no;
         NET_VENDOR_NETERION = no;
         NET_VENDOR_NETRONOME = no;
         NET_VENDOR_NI = no;
         NET_VENDOR_NVIDIA = no;
         NET_VENDOR_OKI = no;
         NET_VENDOR_PACKET_ENGINES = no;
         NET_VENDOR_PENSANDO = no;
         NET_VENDOR_QLOGIC = no;
         NET_VENDOR_QUALCOMM = no;
         NET_VENDOR_RDC = no;
         NET_VENDOR_RENESAS = no;
         NET_VENDOR_ROCKER = no;
         NET_VENDOR_SAMSUNG = no;
         NET_VENDOR_SEEQ = no;
         NET_VENDOR_SOLARFLARE = no;
         NET_VENDOR_SILAN = no;
         NET_VENDOR_SIS = no;
         NET_VENDOR_SMSC = no;
         NET_VENDOR_SOCIONEXT = no;
         NET_VENDOR_STMICRO = no;
         NET_VENDOR_SUN = no;
         NET_VENDOR_SYNOPSYS = no;
         NET_VENDOR_TEHUTI = no;
         NET_VENDOR_TI = no;
         NET_VENDOR_VIA = no;
         NET_VENDOR_WIZNET = no;
         NET_VENDOR_XILINX = no;
         NET_VENDOR_XIRCOM = no;
         WLAN_VENDOR_ADMTEK = no;
         WLAN_VENDOR_ATMEL = no;
         WLAN_VENDOR_BROADCOM = no;
         WLAN_VENDOR_CISCO = no;
         WLAN_VENDOR_INTERSIL = no;
         WLAN_VENDOR_MEDIATEK = no;
         WLAN_VENDOR_MICROCHIP = no;
         WLAN_VENDOR_RALINK = no;
         WLAN_VENDOR_RSI = no;
         WLAN_VENDOR_ST = no;
         WLAN_VENDOR_TI = no;
         WLAN_VENDOR_ZYDAS = no;
         WLAN_VENDOR_QUANTENNA = no;
         INPUT_TOUCHSCREEN = no;
         INPUT_MISC = no;
         WATCHDOG = no;
         HIBERNATION = no;
         };
         ignoreConfigErrors = true;
     });
    })
  ];
}