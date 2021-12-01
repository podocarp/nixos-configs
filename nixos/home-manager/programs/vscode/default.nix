{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    extensions = with pkgs; [
      vscode-extensions.vscodevim.vim
    ];
    userSettings = {
      "editor.minimap.enabled" = false;
      "keyboard.dispatch" = "keyCode";
      "git.enabled" = false;
      "vim.normalModeKeyBindings" = [
        {
          "before" = ["<Esc>"];
          "commands" = ["workbench.action.files.save"];
        }
      ];
      "editor.lineNumbers" = "relative";
      "editor.acceptSuggestionOnCommitCharacter" = false;
      "workbench.activityBar.visible" = false;
    };
  };
}
