{ ... }:
let
  mykey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCo9zWNi53WN8NRWNm2ZwMAVy3YPK7IS9nbKo0hHhy+HYjwwuNx0PJg1XaUuJpbN1nKiHh2UJCRO/OsZNFtLz23abMd41jjiNT5+u2NWYjZYC2uZnqirJXr2VbJDHKWndyrC3EZhDdx6MZ44zDC9LirTZETgHgc75I24HvLLlkSfSVjOlMUe1SP38+gpypruzIEA9olLoQ81UjxWarr1w7E5BWKfzvjuzNVKzf3Yl4t6hxpvvHU4Gg8Yuu7fyf0dmNpC6r+HC4qGNS/3MkZwFiExg+k2ACXS0yBPA+40ANQYsPiEGhTLvpusK4BvstV7AnbRLFdrGLTs6E+2XZCaAK5 openpgp:0x79E90D11";
  surface = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDHhl4m5+u18LmRT+nS1Iva9KJC5rLa291JvDKcKkDv6aGsPKWYD2+Gh00zZxG9vgshqQBlNYwhWXoMkR7MMFvHjhES36SC/Gjn8Wo6gPAp15b2iZm2M565BDGFcb/IuOmzVMzhWZhWcUAbyB/gjsNXuT6LCdaw8FbEkTa4TAw0ozuHgrTVgM5U4QLXpPOLIjICWlWapbkK9fBL36ci482gCGuVSqukVskuGgsy3Mp1tRIgjbpX5xkcSE5AQdxDyl99s1RbTLH3xB90SRPXebYWG4VlMC/1/vRMmCs75tNnhRFeNK472El2gp4bMUI8QqCPMXHP/qBlNuq9Z1Ra8opRHAW4Zyw/4JAkKdYsxMqxfG9U56PcfxabOwCh123mdgSsquKzHwLp3rEMf3ejehUfnsvHQBoXY7UA5/+/iTkz8b8DQgiot1naU5ju6JpITwnLr4ESgkcdyw1Kn4ktOMeGUQcrN9j6iFwZ4bnd0qUUiXiR3p01We2+Fnw8dH8NTik= jxd97@Slate";
  latitudekey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDHCJ1Q7pUuNx7wU3qIYptDc1Nx+3Nql/P+9PPN8bTcHlU/LqBUjXCZqrmZ2sFzfAlzeXeAPxRBPfqnOX7EJygJP3Nk3J2sCqX7+bZp70YyXuAIckrfz2gEq7nXY57b4+8NnQ74LOk7Vrhpb22ab14hHYOM7vO5IruyOJBicpBWstk2V5i+0bpmmLPi8Bn22R5DO5GzKRqAE9wuKvI7DCHP96pAKqjm66s+aGsVnv3qeUH1zhgqVWmKfSXQhokkmpc+UxH3CjKK0WEQFru6j1arRnRqEoeGyp6Y3kRNkI5SyVplI0KwIq0zX08VvD5tBpn91615zKUgqPNPCI5N+ba8zDIdlmhKbBt52JZ8gh6aO8oie3qTsWHzgnK1NGpe83FXze0h2F9p6iXarvKqMuGo+YZUM+m9OhH3kuNARFQWwkjgtCFSp04aF6pTEn8or2pPiea7xMzzrdtp4OG+R8PNTL9FULAH7dWVU9zeoXspUAQad+0gCkf3QxpigZpxXdKpZdwok/XJsS+80SMe6cmz0+FdakiTzi3ludyH+ciRc8ys5jXujrC7mlPUU7hi/EuqoNLuEWYk8ikSguzHPw5p0AqpzrwVdcO0wyJKm6uuCEZVWaajtYsrhJl6fcDbkFH8+hI4qqs31zByzcGUyr4PPHbG5Pz+n4xsgg8nNqMpTw==";
  phonekey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDaq/DH9i6mMGrRST8GcIRQhyvHMzr7mTU6eWdN9ZuPopI47p246WGFfixeV76dupu3HSPK+qA+MpQPahQigsy6UsS5NvmH/C+6H7aH9Es1dVpufkR+pLqwBLmzIP3wj8UALSfpy5sEHA+fsuicQ4EqCxVW517ZbLKd9vyXfAGjMvm7fZQPoBEwZ9tYnOixMD+ddEZa9PgyH48RtWB90cdhDztCTXvFNdHvRWLPvF5MvfAuWjKaUqm0cuAdozZD9u87XEZRBTjXpsJJbOyl2RUGmPExVDZSH0pYLa4THagSOHP7Ky0HtUw58wumpm8fQv9zrhrkhmVodU0gX3dnkcPBLnnbvNApEljxaQBnREkmJ7jEkwefYC2wDyOOrlwP2vcqcyUnfz80b4IUs0qVQTkRIQflxMU5HgeTOkh8QNSic8Ny/y/tfYcikXzsURneOsKWPR3FDCGn8G2gE35VDHrxBNyp0x8xDpWldPdbnuNOdm3tEfofWs880MolM3JMkK1zVlEjH22U/T8ziUM9xsEvKrZ+2c88Bfd+6XAd3vOCNd2YR27X2UltdX5FCmuVZAouCRMwCeq+R8maNqLe0ZACD/d4Aw/IQKuLbokuVTAhXIgK9CREG+ehgmqJBcFFRwcBHI2+tC9aJ+zUTp8BzVPzzucAyxsxUdYXs3a4uyBp6w== u0_a401@localhost";
in
{
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    allowSFTP = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
    extraConfig = ''
      Match Group sftp
        ForceCommand internal-sftp
        AllowTcpForwarding no
    '';
  };

  users.users.pengu.openssh.authorizedKeys.keys = [
    mykey
    surface
    latitudekey
    phonekey
  ];
  users.users.pengu-sftp.openssh.authorizedKeys.keys = [
    mykey
    surface
    latitudekey
    phonekey
  ];
  users.users.root.openssh.authorizedKeys.keys = [
    mykey
  ];
}
