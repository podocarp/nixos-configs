{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    extensions = (with pkgs.vscode-extensions; [
      vscodevim.vim
      golang.go
    ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "activitusbar";
        publisher = "Gruntfuggly";
        version = "0.0.47";
        sha256 = "sha256-hLjkC8hFnRyUaMM2/nN1gKfmT7OlWzmwxfXbxoC0Kpo=";
      }
    ];
    userSettings =
      {
        "editor.minimap.enabled" = false;
        "editor.mouseWheelZoom" = false;
        "keyboard.dispatch" = "keyCode";
        "editor.lineNumbers" = "relative";
        "editor.acceptSuggestionOnCommitCharacter" = false;
        "workbench.activityBar.visible" = false;
        "workbench.colorTheme" = "Default Light+";

        "git.confirmSync" = false;

        "C_Cpp.default.cppStandard" = "c++17";

        "python.analysis.typeCheckingMode" = "basic";

        "vim.normalModeKeyBindings" = [
          {
            "before" = [ "<Esc>" ];
            "commands" = [ "workbench.action.files.save" ];
          }
        ];

        "jupyter.widgetScriptSources" = [ "jsdelivr.com" "unpkg.com" ];
        "jupyter.askForKernelRestart" = false;

        "eslint.format.enable" = true;
        "eslint.lintTask.enable" = true;
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
