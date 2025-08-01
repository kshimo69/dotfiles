# dotfiles

This is my dotfiles repository

## Supported environments

- Mac OSX
- RHEL (CentOS, Amazon Linux)
- Ubuntu

## Prerequisite

- [ansible](http://www.ansible.com/home)

### Mac

- Install Xcode

```bash
$ xcode-select --install
```

- Install [homebrew](https://brew.sh/)

```bash
$ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

- Install [ansible](http://www.ansible.com/home)

```bash
$ brew install ansible
$ sudo launchctl limit maxfiles unlimited
```

### RHEL

- Install EPEL

```bash
$ sudo yum install -y epel-release
```

- Install git

```bash
$ sudo yum install -y --enablerepo=epel git
```

- Install ansible

```bash
$ sudo yum install -y --enablerepo=epel ansible
```

### Ubuntu

- Install [homebrew](https://brew.sh/)

```bash
$ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

- Install [ansible](http://www.ansible.com/home)

```bash
$ brew install ansible
```

## How to use

- Install

```bash
$ curl -LSfs https://raw.githubusercontent.com/kshimo69/dotfiles/master/install.sh | bash
```

- Update

```bash
$ ansible-playbook --force-handlers ./playbooks/local.yml
```

- Execute specific roles

```bash
$ ansible-playbook --force-handlers ./playbooks/local.yml --tags "common"
```

## After Install...

- set default shell

```bash
`which zsh` | sudo tee -a /etc/shells
chsh -s `which zsh`
```

- update neovim plugins

```vim
:call dein#update()
```

```vim
:UpdateRemotePlugins
```

restart neovim

- edit ~/.gitconfig.local

for home or for office.

```
[user]
    email = username@example.com
    name = Your Name
[github]
    user = username
    token = XXXXXXXX
```

- edit ~/.netrc

```
machine github.com
login USERNAME
password PASSWORD
```

- edit ~/.passwd

This file load from .zshrc.
password or proxy config in office.

```
export http_proxy=http://proxy.example:8080/
export https_proxy=$http_proxy
export HTTP_PROXY=$http_proxy
export HTTPS_PROXY=$http_proxy
export ftp_ftp=$http_proxy
export FTP_PROXY=$http_proxy
export no_proxy=localhost,127.0.0.1
export NO_PROXY=$no_proxy
```

- edit ~/.ssh/config

```
Host *
    Compression yes
    CompressionLevel 9
    # Ciphers arcfour256
Host ssh-proxy
    HostName proxy-host.example.com
    User USERNAME
    IdentitiesOnly yes
    IdentityFile ~/.ssh/id_rsa.specified
    StrictHostKeyChecking no
Host *.example.com
    User USERNAME
    IdentityFile ~/.ssh/id_rsa.specified
    ProxyCommand ssh -q ssh-proxy nc %h %p
    StrictHostKeyChecking no
```

- edit ~/.subversion/servers

Set proxy config.
Some brew package use subversion.
