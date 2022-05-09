{ ... }:
{
  programs.sioyek = {
    enable = true;
    config = {
      "check_for_updates_on_startup" = "0";
      "default_dark_mode" = "0";
      "background_color" = "0 0 0";
      "sort_bookmarks_by_location" = "1";
      # "inverse_search_command" = "nvr --remote-silent %1 -c %2";
      "single_main_window_size" = "1205 1705";
    };
    bindings = {
      "goto_begining" = "gg";
      "goto_end" = "<S-g>";
      "move_up" = "k";
      "move_down" = "j";
      "move_left" = "l";
      "move_right" = "h";
      "next_page" = "<C-d>";
      "previous_page" ="<C-u>";
      "prev_state" = "<C-o>";
      "next_state" = "<C-i>";
      "goto_toc" = "<Tab>";
      "zoom_in" = "=";
      "zoom_out" = "-";
      "fit_to_page_width" = "w";
      "fit_to_page_width_smart" = "e";
      "search" = "/";
      "next_item" = "n";
      "previous_item" = "<S-n>";
      "add_bookmark" = "b";
      "delete_bookmark" = "db";
      "goto_bookmark" = "gb";
      "goto_bookmark_g" = "g<S-b>";
      "add_highlight" = "<S-h>";
      "set_mark" = "m";
      "goto_mark" = "'";
      "link" = "f";
      "delete_link" = "df";
      "quit" = "q";
    };
  };
}
