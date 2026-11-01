# LTeXConfigManipulator
A command-line tool for managing project-specific LTeX+ configurations with a focus on Vim integration.

## Description
LTeXConfigManipulator is a command-line utility designed to integrate LTeX+ with Vim.
It manages project-specific language settings and custom dictionary entries, allowing Vim to load the appropriate configuration for each project.

## Features
- **Project-specific configuration**: Manage LTeX+ settings separately for each project.
- **Language configuration**: Set the language used by LTeX+ for a project.
- **Custom dictionary management**: Add words to or remove words from the project's dictionary.
- **Vim integration**: Load project-specific language and dictionary settings through `vim-lsp`.

### Project Root Detection
The project root is determined from the TeX file specified with `--texFile` command line option.
Starting from the file's directory, the tool searches upwards (towards the file system root) for the following markers, in order of priority:
1. `.ltex-root` file or `.ltex/` directory: Identifies and LTeX project root.
2. `.git/` directory or `.git` file: Used as a fallback project root.
3. **Fallback**: If neither marker is found, the directory containing the specified TeX file is used. 

## Usage
### Installation
You can install LTeXConfigManipulator in either of the following ways:
- Download a prebuilt executable from the [Releases](https://github.com/t-sasaki915/LTeXConfigManipulator/releases) page.
- Build and install from source using Stack:
  ```
  stack install
  ```
### Command-Line Interface
The following command displays the available commands and options:
```
LTeXConfigManipulator --help
```
Each command has its own help message. For example:
```
LTeXConfigManipulator language --help
```
The mandatory `--texFile` option specifies a TeX file used to determine the project root.
The tool uses this file to locate the project-specific configuration.

#### Examples
Display the project root:
```
LTeXConfigManipulator projectRoot --texFile path/to/document.tex
```
Show the current language setting:
```
LTeXConfigManipulator show language --texFile path/to/document.tex
```
Set the language `ja-JP` for a project:
```
LTeXConfigManipulator language ja-JP --texFile path/to/document.tex
```
Register a word `myword` in the custom dictionary:
```
LTeXConfigManipulator correctWord myword --texFile path/to/document.tex
```
Remove a word `myword` from the custom dictionary:
```
LTeXConfigManipulator wrongWord myword --texFile path/to/document.tex
```

## Vim Integration
LTeXConfigManipulator was designed primarily for use with Vim and `vim-lsp`.
The goal of LTeXConfigManipulator is to keep the Vim script integration simple.
The key to the integration is passing the path of the currently edited TeX file to every command using the `--texFile` option:
```vim
system('LTeXConfigManipulator <command> --texFile ' . shellescape(expand('%:p')))
```
Vim script does not need to handle project root detection, JSON parsing or configuration management itself.
Instead, these tasks are delegated to LTeXConfigManipulator, a Haskell-based tool designed to handle them in a structured and reliable way.
That is, Vim script can dedicate to invoke the tool through `system()` and provide the result to `vim-lsp` as workspace configuration.
This keeps the Vim script integration lightweight while leaving the more complex logic to a dedicated tool.

### Example
The following Vim script shows how to configure `vim-lsp` to load project-specific LTeX+ settings and provide commands for managing the custom dictionary (`:CorrectWord` and `:WrongWord`) and language (`:LTeXSetLanguage <lang>`).
```vim
if executable('ltex-ls-plus') && executable('LTeXConfigManipulator')
    function! s:LtexWorkspaceConfig(...) abort
        let l:texfile = expand('%:p')

        let l:config = {
                    \ 'ltex': {
                    \     'enabled': ['latex', 'tex', 'bib']
                    \ }
                    \ }

        let l:language = trim(system('LTeXConfigManipulator show language --texFile ' . shellescape(l:texfile)))
        if v:shell_error != 0 || empty(l:language)
            let l:language = 'en-GB'
        endif

        let l:config.ltex.language = l:language
        
        let l:words = systemlist('LTeXConfigManipulator show dictionary --texFile ' . shellescape(l:texfile))
        if v:shell_error == 0
            call map(l:words, 'substitute(v:val, "\r$", "", "")')
            call filter(l:words, 'v:val !~# "^\\s*$"')
            
            let l:config.ltex.dictionary = {
                        \ l:language: l:words
                        \ }
        endif

        return l:config
    endfunction

    function! s:LTeXCorrectWord() abort
        let l:word = expand('<cword>')
        if empty(l:word)
            echo 'No word under cursor.'
            return
        endif

        echo trim(system('LTeXConfigManipulator correctWord ' . shellescape(l:word) . ' --texFile ' . shellescape(expand('%:p'))))
    endfunction

    function! s:LTeXWrongWord() abort
        let l:word = expand('<cword>')
        if empty(l:word)
            echo 'No word under cursor.'
            return
        endif

        echo trim(system('LTeXConfigManipulator wrongWord ' . shellescape(l:word) . ' --texFile ' . shellescape(expand('%:p'))))
    endfunction

    function! s:LTeXSetLanguage(language) abort
        echo trim(system('LTeXConfigManipulator language ' . shellescape(a:language) . ' --texFile ' . shellescape(expand('%:p'))))
    endfunction

    let s:ltexExecName = has('win32') ? 'ltex-ls-plus.bat' : 'ltex-ls-plus'
    autocmd User lsp_setup call lsp#register_server({
                \ 'name': 'ltex-ls-plus',
                \ 'cmd': {server_info -> [s:ltexExecName]},
                \ 'allowlist': ['tex', 'bib'],
                \ 'workspace_config': function('s:LtexWorkspaceConfig'),
                \ })

    command! CorrectWord call s:LTeXCorrectWord()
    command! WrongWord call s:LTeXWrongWord()
    command! -nargs=1 LTeXSetLanguage call s:LTeXSetLanguage(<q-args>)
endif
```
