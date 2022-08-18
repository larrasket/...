# neovim-config
A simple neovim configuration with native LSP and treesitter support.

# Installation
You should use the most recent version of neovim

If you have an already existing neovim, then pick what do you want from this config,

If you are new follow these steps:
1. clone this repo into neovim config-location:
```bash 
   mkdir -p ~/.config/nvim
   git clone https://github.com/aboueleyes/neovim-config ~/.config/nvim
   ```
2. install vim-plug:

   ```bash 
   sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
          https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
   ```
3. install plugins:
``` vimscript
nvim '+PlugInstall | qa'
```

4. start `nvim`
5. Install LSPs for the languages you care about via eg :LspInstall python. You can use tab completion after typing :LspInstall to see which language servers are available.
