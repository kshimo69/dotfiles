# dotfiles

This is my dotfiles repository

## Supported environments

- Mac OSX
- RHEL (CentOS, Amazon Linux)

## Prerequisite

- [ansible](http://www.ansible.com/home)

### How to install ansible

#### Linux

- Install EPEL

```bash
$ sudo yum install -y epel-release
```

- Install git

```bash
$ sudo yum install -y git
```

- Install ansible

```bash
$ sudo yum -y install ansible
```

#### Mac

- Install Xcode

```bash
$ xcode-select --install
```

- Install [homebrew](https://brew.sh/)

```bash
$ ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
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
* Mac
ansible-playbook ./playbooks/default/mac.yml
* RedHat Linux (Amazon Linux)
ansible-playbook ./playbooks/default/rhel.yml
```

## After Install...

- edit hogehoge

- update neovim plugins

```vim
:call dein#update()
```

edit .py file

```vim
:UpdateRemotePlugins
```
