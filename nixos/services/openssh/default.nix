{ ... }:
let
  mykey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCo9zWNi53WN8NRWNm2ZwMAVy3YPK7IS9nbKo0hHhy+HYjwwuNx0PJg1XaUuJpbN1nKiHh2UJCRO/OsZNFtLz23abMd41jjiNT5+u2NWYjZYC2uZnqirJXr2VbJDHKWndyrC3EZhDdx6MZ44zDC9LirTZETgHgc75I24HvLLlkSfSVjOlMUe1SP38+gpypruzIEA9olLoQ81UjxWarr1w7E5BWKfzvjuzNVKzf3Yl4t6hxpvvHU4Gg8Yuu7fyf0dmNpC6r+HC4qGNS/3MkZwFiExg+k2ACXS0yBPA+40ANQYsPiEGhTLvpusK4BvstV7AnbRLFdrGLTs6E+2XZCaAK5 openpgp:0x79E90D11";
  latitudekey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDHCJ1Q7pUuNx7wU3qIYptDc1Nx+3Nql/P+9PPN8bTcHlU/LqBUjXCZqrmZ2sFzfAlzeXeAPxRBPfqnOX7EJygJP3Nk3J2sCqX7+bZp70YyXuAIckrfz2gEq7nXY57b4+8NnQ74LOk7Vrhpb22ab14hHYOM7vO5IruyOJBicpBWstk2V5i+0bpmmLPi8Bn22R5DO5GzKRqAE9wuKvI7DCHP96pAKqjm66s+aGsVnv3qeUH1zhgqVWmKfSXQhokkmpc+UxH3CjKK0WEQFru6j1arRnRqEoeGyp6Y3kRNkI5SyVplI0KwIq0zX08VvD5tBpn91615zKUgqPNPCI5N+ba8zDIdlmhKbBt52JZ8gh6aO8oie3qTsWHzgnK1NGpe83FXze0h2F9p6iXarvKqMuGo+YZUM+m9OhH3kuNARFQWwkjgtCFSp04aF6pTEn8or2pPiea7xMzzrdtp4OG+R8PNTL9FULAH7dWVU9zeoXspUAQad+0gCkf3QxpigZpxXdKpZdwok/XJsS+80SMe6cmz0+FdakiTzi3ludyH+ciRc8ys5jXujrC7mlPUU7hi/EuqoNLuEWYk8ikSguzHPw5p0AqpzrwVdcO0wyJKm6uuCEZVWaajtYsrhJl6fcDbkFH8+hI4qqs31zByzcGUyr4PPHbG5Pz+n4xsgg8nNqMpTw=="
in
{
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    kbdInteractiveAuthentication = false;
  };

  users.users.pengu.openssh.authorizedKeys.keys = [
    mykey
    latitudekey
  ];
  users.users.root.openssh.authorizedKeys.keys = [
    mykey
    latitudekey
  ];
}
