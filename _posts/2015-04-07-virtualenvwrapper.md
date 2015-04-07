---
layout: post
title: "virtualenvwrapper"
description: ""
category: python
tags: [python, virtualenvwrapper]
---
{% include JB/setup %}

## Mac OS X
---

`sudo pip install virtualenvwrapper`

`cat ~/.bashrc`

> export WORKON_HOME=$HOME/.virtualenvs
>
> export PROJECT_HOME=$HOME/Devel
>
> source /usr/local/bin/virtualenvwrapper.sh

`source ~/.bashrc`

`mkvirtualenv temp`

`(temp)$ pip install django`

`deactivate`

`echo 'cd $VIRTUAL_ENV' >> $WORKON_HOME/postactivate`

`workon`

> temp

`workon temp`

`lssitepackages`

> Django-1.8.dist-info/      pip/
>
> _markerlib/                pip-6.1.0.dist-info/
>
> django/                    pkg_resources/
>
> easy_install.py            setuptools/
>
> easy_install.pyc           setuptools-15.0.dist-info/


## virtualenvwrapper 4.3.2.post5
---

<http://virtualenvwrapper.readthedocs.org/en/latest/>

<http://virtualenvwrapper.readthedocs.org/en/latest/install.html>

virtualenvwrapper is a set of extensions to Ian Bicking’s virtualenv tool. The extensions include wrappers for creating and deleting virtual environments and otherwise managing your development workflow, making it easier to work on more than one project at a time without introducing conflicts in their dependencies.

### Features

1. Organizes all of your virtual environments in one place.
1. Wrappers for managing your virtual environments (create, delete, copy).
1. Use a single command to switch between environments.
1. Tab completion for commands that take a virtual environment as argument.
1. User-configurable hooks for all operations (see Per-User Customization).
1. Plugin system for more creating sharable extensions (see Extending Virtualenvwrapper).

### Introduction

The best way to explain the features virtualenvwrapper gives you is to show it in use.

First, some initialization steps. Most of this only needs to be done one time. You will want to add the command to source /usr/local/bin/virtualenvwrapper.sh to your shell startup file, changing the path to virtualenvwrapper.sh depending on where it was installed by pip.

> $ pip install virtualenvwrapper
>
> ...
>
> $ export WORKON_HOME=~/Envs
>
> $ mkdir -p $WORKON_HOME
>
> $ source /usr/local/bin/virtualenvwrapper.sh
>
> $ mkvirtualenv env1
>
> Installing
>
> setuptools..........................................
>
> ....................................................
>
> ....................................................
>
> ...............................done.
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env1/bin/predeactivate
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env1/bin/postdeactivate
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env1/bin/preactivate
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env1/bin/postactivate  New python executable in env1/bin/python
>
> (env1)$ ls $WORKON_HOME
>
> env1 hook.log
>

Now we can install some software into the environment.

> (env1)$ pip install django
>
> Downloading/unpacking django
>
>   Downloading Django-1.1.1.tar.gz (5.6Mb): 5.6Mb downloaded
>
>   Running setup.py egg_info for package django
>
> Installing collected packages: django
>
>   Running setup.py install for django
>
>     changing mode of build/scripts-2.6/django-admin.py from 644 to 755
>
>     changing mode of /Users/dhellmann/Envs/env1/bin/django-admin.py to 755
>
> Successfully installed django
>

We can see the new package with lssitepackages:

> (env1)$ lssitepackages
>
> Django-1.1.1-py2.6.egg-info     easy-install.pth
>
> setuptools-0.6.10-py2.6.egg     pip-0.6.3-py2.6.egg
>
> django                          setuptools.pth
>

Of course we are not limited to a single virtualenv:

> (env1)$ ls $WORKON_HOME
>
> env1            hook.log
>
> (env1)$ mkvirtualenv env2
>
> Installing setuptools...............................
>
> ....................................................
>
> ....................................................
>
> ........... ...............................done.
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env2/bin/predeactivate
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env2/bin/postdeactivate
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env2/bin/preactivate
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env2/bin/postactivate  New python executable in env2/bin/python
>
> (env2)$ ls $WORKON_HOME
>
> env1            env2            hook.log

Switch between environments with workon:

>
> (env2)$ workon env1
> (env1)$ echo $VIRTUAL_ENV
> /Users/dhellmann/Envs/env1
> (env1)$

The workon command also includes tab completion for the environment names, and invokes customization scripts as an environment is activated or deactivated (see Per-User Customization).

> (env1)$ echo 'cd $VIRTUAL_ENV' >> $WORKON_HOME/postactivate
>
> (env1)$ workon env2
>
> (env2)$ pwd
>
> /Users/dhellmann/Envs/env2
>

postmkvirtualenv is run when a new environment is created, letting you automatically install commonly-used tools.

> (env2)$ echo 'pip install sphinx' >> $WORKON_HOME/postmkvirtualenv
>
> (env3)$ mkvirtualenv env3
>
> New python executable in env3/bin/python
>
> Installing setuptools...............................
>
> ....................................................
>
> ....................................................
>
> ........... ...............................done.
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env3/bin/predeactivate
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env3/bin/postdeactivate
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env3/bin/preactivate
>
> virtualenvwrapper.user_scripts Creating /Users/dhellmann/Envs/env3/bin/postactivate
>
> Downloading/unpacking sphinx
>
>   Downloading Sphinx-0.6.5.tar.gz (972Kb): 972Kb downloaded
>
>   Running setup.py egg_info for package sphinx
>
>     no previously-included directories found matching 'doc/_build'
>
> Downloading/unpacking Pygments>=0.8 (from sphinx)
>
>   Downloading Pygments-1.3.1.tar.gz (1.1Mb): 1.1Mb downloaded
>
>   Running setup.py egg_info for package Pygments
>
> Downloading/unpacking Jinja2>=2.1 (from sphinx)
>
>   Downloading Jinja2-2.4.tar.gz (688Kb): 688Kb downloaded
>
>   Running setup.py egg_info for package Jinja2
>
>     warning: no previously-included files matching '*' found under directory 'docs/_build/doctrees'
>
> Downloading/unpacking docutils>=0.4 (from sphinx)
>
>   Downloading docutils-0.6.tar.gz (1.4Mb): 1.4Mb downloaded
>
>   Running setup.py egg_info for package docutils
>
> Installing collected packages: docutils, Jinja2, Pygments, sphinx
>
>   Running setup.py install for docutils
>
>   Running setup.py install for Jinja2
>
>   Running setup.py install for Pygments
>
>   Running setup.py install for sphinx
>
>     no previously-included directories found matching 'doc/_build'
>
>     Installing sphinx-build script to /Users/dhellmann/Envs/env3/bin
>
>     Installing sphinx-quickstart script to /Users/dhellmann/Envs/env3/bin
>
>     Installing sphinx-autogen script to /Users/dhellmann/Envs/env3/bin
>
> Successfully installed docutils Jinja2 Pygments sphinx  (env3)$
>
> (venv3)$ which sphinx-build
>
> /Users/dhellmann/Envs/env3/bin/sphinx-build

Through a combination of the existing functions defined by the core package (see Command Reference), third-party plugins (see Extending Virtualenvwrapper), and user-defined scripts (see Per-User Customization) virtualenvwrapper gives you a wide variety of opportunities to automate repetitive operations.

