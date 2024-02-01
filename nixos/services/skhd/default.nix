{ ... }:
{
  services.skhd = {
    enable = true;
    skhdConfig = ''
      # taken from https://github.com/koekeishiya/yabai/issues/295
      # focus window
      # here the || was added so the selection cycles and doesn't stop at the end or beginning
      alt - j : yabai -m window --focus prev || yabai -m window --focus last
      alt - k : yabai -m window --focus next || yabai -m window --focus first

      # swap window
      alt - return : yabai -m window --swap west # swap with "main" tile (simply swap it west)
      shift + alt - j : yabai -m window --swap prev
      shift + alt - k : yabai -m window --swap next

      # move window
      shift + cmd - h : yabai -m window --warp west
      shift + cmd - j : yabai -m window --warp north
      shift + cmd - k : yabai -m window --warp next
      shift + cmd - l : yabai -m window --warp east

      # balance size of windows
      shift + alt - 0 : yabai -m space --balance

      # send window to desktop and follow focus
      shift + alt - 1 : yabai -m window --space  1
      shift + alt - 2 : yabai -m window --space  2
      shift + alt - 3 : yabai -m window --space  3
      shift + alt - 4 : yabai -m window --space  4
      shift + alt - 5 : yabai -m window --space  5
      shift + alt - 6 : yabai -m window --space  6
      shift + alt - 7 : yabai -m window --space  7
      shift + alt - 8 : yabai -m window --space  8
      shift + alt - 9 : yabai -m window --space  9
      shift + alt - 0 : yabai -m window --space 10

      # increase window size (this is the hack that gives xmonad like resizing)
      alt - h : expr $(yabai -m query --windows --window | jq .frame.x) \< 20 && yabai -m window --resize right:-60:0 || yabai -m window --resize left:-60:0
      alt - l : expr $(yabai -m query --windows --window | jq .frame.x) \< 20 && yabai -m window --resize right:60:0 || yabai -m window --resize left:60:0
      alt - i : yabai -m window --resize bottom:0:-60
      alt - o : yabai -m window --resize bottom:0:60

      # rotate tree
      alt - space : yabai -m space --rotate 90

      # mirror tree y-axis
      alt - y : yabai -m space --mirror y-axis

      # mirror tree x-axis
      alt - x : yabai -m space --mirror x-axis

      # toggle window fullscreen zoom
      alt - f: yabai -m window --toggle zoom-fullscreen

      # toggle window native fullscreen
      # alt - space : yabai -m window --toggle native-fullscreen
      shift + alt - f : yabai -m window --toggle native-fullscreen

      # toggle window border
      # shift + alt - b : yabai -m window --toggle border

      # toggle window split type
      alt - e : yabai -m window --toggle split

      # float / unfloat window and center on screen
      alt - t : yabai -m window --toggle float;\
                yabai -m window --grid 4:4:1:1:2:2
    '';
  };
}
