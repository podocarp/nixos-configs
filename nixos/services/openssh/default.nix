{ ... }:
let
  mykey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCo9zWNi53WN8NRWNm2ZwMAVy3YPK7IS9nbKo0hHhy+HYjwwuNx0PJg1XaUuJpbN1nKiHh2UJCRO/OsZNFtLz23abMd41jjiNT5+u2NWYjZYC2uZnqirJXr2VbJDHKWndyrC3EZhDdx6MZ44zDC9LirTZETgHgc75I24HvLLlkSfSVjOlMUe1SP38+gpypruzIEA9olLoQ81UjxWarr1w7E5BWKfzvjuzNVKzf3Yl4t6hxpvvHU4Gg8Yuu7fyf0dmNpC6r+HC4qGNS/3MkZwFiExg+k2ACXS0yBPA+40ANQYsPiEGhTLvpusK4BvstV7AnbRLFdrGLTs6E+2XZCaAK5 openpgp:0x79E90D11";
  mac = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICAaqI+9tVRP8m02J6Iu3MPBcnyKpyyOBLTUTuHRyWYI xiaodong.jia@bytedance.com";
  phonekey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDaq/DH9i6mMGrRST8GcIRQhyvHMzr7mTU6eWdN9ZuPopI47p246WGFfixeV76dupu3HSPK+qA+MpQPahQigsy6UsS5NvmH/C+6H7aH9Es1dVpufkR+pLqwBLmzIP3wj8UALSfpy5sEHA+fsuicQ4EqCxVW517ZbLKd9vyXfAGjMvm7fZQPoBEwZ9tYnOixMD+ddEZa9PgyH48RtWB90cdhDztCTXvFNdHvRWLPvF5MvfAuWjKaUqm0cuAdozZD9u87XEZRBTjXpsJJbOyl2RUGmPExVDZSH0pYLa4THagSOHP7Ky0HtUw58wumpm8fQv9zrhrkhmVodU0gX3dnkcPBLnnbvNApEljxaQBnREkmJ7jEkwefYC2wDyOOrlwP2vcqcyUnfz80b4IUs0qVQTkRIQflxMU5HgeTOkh8QNSic8Ny/y/tfYcikXzsURneOsKWPR3FDCGn8G2gE35VDHrxBNyp0x8xDpWldPdbnuNOdm3tEfofWs880MolM3JMkK1zVlEjH22U/T8ziUM9xsEvKrZ+2c88Bfd+6XAd3vOCNd2YR27X2UltdX5FCmuVZAouCRMwCeq+R8maNqLe0ZACD/d4Aw/IQKuLbokuVTAhXIgK9CREG+ehgmqJBcFFRwcBHI2+tC9aJ+zUTp8BzVPzzucAyxsxUdYXs3a4uyBp6w== u0_a401@localhost";
in
{
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

  users.users.pengu.openssh.authorizedKeys.keys = [
    mac
    mykey
    phonekey
  ];
  users.users.root.openssh.authorizedKeys.keys = [
    mykey
  ];
}
