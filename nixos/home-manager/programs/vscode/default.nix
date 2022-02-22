{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      vscodevim.vim
      ms-python.python
      ms-vscode.cpptools
    ];
    userSettings = {
      "editor.minimap.enabled" = false;
      "keyboard.dispatch" = "keyCode";
      "editor.lineNumbers" = "relative";
      "editor.acceptSuggestionOnCommitCharacter" = false;
      "workbench.activityBar.visible" = false;
      "workbench.colorTheme" = "Default Light+";

      "C_Cpp.default.cppStandard" = "c++17";

      "vim.normalModeKeyBindings" = [
        {
          "before" = ["<Esc>"];
          "commands" = ["workbench.action.files.save"];
        }
      ];
    };
    keybindings = [
      {
        key = "ctrl+tab";
        command = "workbench.action.nextEditor";
      }
      {
        key = "ctrl+shift+tab";
        command = "workbench.action.previousEditor";
      }
    ];
  };
}
