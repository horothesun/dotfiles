# dotfiles

## Dependencies

- GNU Stow ([reference video](https://www.youtube.com/watch?v=CFzEuBGPPPg))

## Setup

```bash
cd ~ ; git clone git@github.com:horothesun/dotfiles.git ; cd dotfiles
```

## Examples

### Stow

```bash
stow --simulate --verbose --target ~ alacritty
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
