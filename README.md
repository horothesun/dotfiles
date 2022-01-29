# dotfiles

## Dependencies

- GNU Stow ([reference video](https://www.youtube.com/watch?v=CFzEuBGPPPg))

## Setup

```bash
cd ~ ; git clone git@github.com:horothesun/dotfiles.git ; cd dotfiles
```

## Alacritty specific

After using `stow`, run

```bash
cp alacritty/alacritty_new_window_same_dir.sh /usr/local/bin/alacritty_new_window_same_dir.sh
```

> Note: creating a symbolic link to the bash script doesn't work.

## Examples

### Stow

```bash
stow --simulate --no-folding --verbose --target ~ alacritty
```

### Re-stow

```bash
stow --simulate --no-folding --restow --verbose --target ~ alacritty
```

### Unstow

```bash
stow --simulate --verbose --delete --target ~ alacritty
```

### Adopt

First create same `~`-based `<APP>` folder _and file structure_ in `dotfiles/`, then

```bash
stow --simulate --adopt --verbose --target ~ <APP>
```
