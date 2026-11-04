# LTeXConfigManipulator
A CLI tool for managing project-specific LTeX+ configurations and integrating them with editors.

## Description
LTeXConfigManipulator is a command-line tool for managing project-specific configurations of LTeX+.
It automatically detects the project root from a TeX (or a BibTeX) file and provides commands to inspect and modify LTeX+ settings within that project.
It supports retrieving and updating configuration values, as well as adding and removing entries from list-based settings such as custom dictionaries.
LTeXConfigManipulator can also generate workspace configuration for LTeX+, making it easier to integrate project-specific settings into editors and language-server clients such as Vim with `vim-lsp`.

## Features
- **Project-specific configuration**: Automatically detects the project root from a TeX file and manages LTeX+ settings for the project.
- **Configuration management**: Retrieve and update individual LTeX+ configuration values using `get` and `set` commands.
- **List management**: Add and remove entries in list-based settings, such as custom dictionaries, using `add` and `remove` commands.
- **Language-specific configuration management**: Use the `currentLang` path to access and modify language-specific settings for the language currently configured in LTeX+.
- **Workspace configuration generation**: Generate LTeX+ workspace configuration in JSON format for use by editors and language-server clients.
- **Vim integration**: Designed to work with Vim and `vim-lsp`, allowing project-specific LTeX+ settings and dictionary entries to be managed from within the editor.

### Project Root Detection
The project root is determined from the TeX file specified with `--texFile` command line option.
Starting from the file's directory, the tool searches upwards (towards the file system root) for the following markers, in order of priority:
1. `.ltex-root` file or `.ltex/` directory: Identifies and LTeX project root.
2. `.git/` directory or `.git` file: Used as a fallback project root.
3. **Fallback**: If neither marker is found, the directory containing the specified TeX file is used. 

## Usage
### Installation
You can install LTeXConfigManipulator in either of the following ways:
- **Download a prebuilt executable** from the [Releases](https://github.com/t-sasaki915/LTeXConfigManipulator/releases) page.
  Prebuilt executables are currently provided for Windows (x86_64) and Linux (x86_64).
- **Build and install from source** using Stack:
  ```
  stack install
  ```
  This option can also be used to build LTeXConfigManipulator on platforms for which prebuilt executables are not available in Releases page.
### Command-Line Interface
To display the available commands and options, run:
```
$ LTeXConfigManipulator --help
Usage: LTeXConfigManipulator COMMAND

  A command-line tool for managing project-specific LTeX+ configurations with a
  focus on Vim integration.

Available options:
  -h,--help                Show this help text

Available commands:
  get                      Get a configuration value
  set                      Set a configuration value
  add                      Add a configuration value
  remove                   Remove a configuration value
  projectRoot              Show a calculated project root path
  workspaceConfig          Show current workspace config in json
```
Each command provides its own help message. For instance,
```
$ LTeXConfigManipulator set --help
Usage: LTeXConfigManipulator set --texFile FILEPATH CONFIG_PATH VALUE

  Set a configuration value

Available options:
  --texFile FILEPATH       The path to the .tex (or .bib) file that you are
                           currently editing
  -h,--help                Show this help text
```
The `--texFile` option specifies a TeX file used to determine the project root.
Commands that operate on project-specific settings use this file to locate the corresponding configuration file.
### Examples
Display the project root:
```
$ LTeXConfigManipulator projectRoot --texFile path/to/document.tex
path/to/project
```
Generate the LTeX+ workspace configuration in JSON format:
```
$ LTeXConfigManipulator workspaceConfig --texFile path/to/document.tex
{
  "ltex": {
    "language": "en-GB",
    "dictionary": {
      "en-GB": [
        "myword"
      ]
    }
  }
}
```
*Note*: The actual output is compact JSON; it is formatted here for readability.
<br><br>
Retrieve the current language setting:
```
$ LTeXConfigManipulator get language --texFile path/to/document.tex
en-GB
```
Set the language to `ja-JP`:
```
$ LTeXConfigManipulator set language ja-JP --texFile path/to/document.tex
```
Add a word to the custom dictionary for the currently configured language:
```
$ LTeXConfigManipulator add dictionary.currentLang myword --texFile path/to/document.tex
```
Remove a word from the custom dictionary for the currently configured language:
```
$ LTeXConfigManipulator remove dictionary.currentLang myword --texFile path/to/document.tex
```

## Vim Integration
LTeXConfigManipulator is primarily designed to be integrated into editors such as Vim, rather than being used only as a standalone command-line tool.
<br><br>
When used with `vim-lsp`, LTeXConfigManipulator provides project-specific workspace configuration for LTeX+.
The `workspaceConfig` command generates the complete configuration JSON expected by LTeX+.
Hence, Vimscript does not need to construct or modify the configuration manually.
<br><br>
For example, the workspace configuration callback can be implemented with just a few lines.
This integration has been tested with standard Vim 9.2 and the `vim-lsp` plugin, without requiring Neovim or a dedicated Vim plugin.
```vim
function! s:LTeXWorkspaceConfig(...) abort
    return json_decode(system(
                \ 'LTeXConfigManipulator workspaceConfig --texFile '
                \ . shellescape(expand('%:p'))))
endfunction

" This LTeXWorkspaceConfig function can be integrated to vim-lsp like so:
let s:ltexExecName = has('win32') ? 'ltex-ls-plus.bat' : 'ltex-ls-plus'
autocmd User lsp_setup call lsp#register_server({
            \ 'name': 'ltex-ls-plus',
            \ 'cmd': {server_info -> [s:ltexExecName]},
            \ 'allowlist': ['tex', 'bib'],
            \ 'workspace_config': function('s:LTeXWorkspaceConfig'),
            \ })
```
LTeXConfigManipulator also provides easier way to manage settings directly from Vim commands.
The following example defines commands for adding and removing words from the dictionary and changing the language:
```vim
function! s:LTeXCorrectWord() abort
    let l:word = expand('<cword>')
    if empty(l:word)
        echo 'No word under cursor.'
        return
    endif

    echo trim(system('LTeXConfigManipulator add dictionary.currentLang '
                \ . shellescape(l:word) . ' --texFile '
                \ . shellescape(expand('%:p'))))
endfunction

function! s:LTeXWrongWord() abort
    let l:word = expand('<cword>')
    if empty(l:word)
        echo 'No word under cursor.'
        return
    endif

    echo trim(system('LTeXConfigManipulator remove dictionary.currentLang '
                \ . shellescape(l:word) . ' --texFile '
                \ . shellescape(expand('%:p'))))
endfunction

function! s:LTeXSetLanguage(language) abort
    echo trim(system('LTeXConfigManipulator set language '
                \ . shellescape(a:language) . ' --texFile '
                \ . shellescape(expand('%:p'))))
endfunction

command! CorrectWord call s:LTeXCorrectWord()
command! WrongWord call s:LTeXWrongWord()
command! -nargs=1 LTeXSetLanguage call s:LTeXSetLanguage(<q-args>)
```
This approach keeps the Vimscript integration relatively simple while leaving configuration handling to LTeXConfigManipulator.
<br><br>
Unlike a general-purpose JSON editor, LTeXConfigManipulator understands the structure of LTeX+ configuration and provides dedicated operations for accessing and modifying individual settings.
<br><br>
Although its functionality is available through a command-line interface, its primary goal is to serve as a configuration management component for editor integrations.
