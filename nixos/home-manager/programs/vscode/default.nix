{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    extensions = (with pkgs.vscode-extensions; [
      vscodevim.vim
    ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "activitusbar";
        publisher = "Gruntfuggly";
        version = "0.0.46";
        sha256 = "sha256-sJEB9IJKIfBlSuhtecMHXlCeJFqh4+fWtOKJlzJ1t+A=";
      }
    ];
    userSettings = {
      "editor.minimap.enabled" = false;
      "keyboard.dispatch" = "keyCode";
      "editor.lineNumbers" = "relative";
      "editor.acceptSuggestionOnCommitCharacter" = false;
      "workbench.activityBar.visible" = false;
      "workbench.colorTheme" = "Default Light+";

      "C_Cpp.default.cppStandard" = "c++17";

      "python.analysis.typeCheckingMode" = "basic";

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
