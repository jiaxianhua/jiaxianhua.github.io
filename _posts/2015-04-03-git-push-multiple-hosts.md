---
layout: post
title: "git push multiple hosts"
description: ""
category: 
tags: [git]
---
{% include JB/setup %}

---

bash

>    `$ cat .git/config`
>
>     [remote "origin"]
>         url = ssh://551a7df75973ca0c1d00012a@markdown-jiaxianhua.rhcloud.com/~/git/ markdown.git/
>         fetch = +refs/heads/*:refs/remotes/origin/*
>
>    `$ vim .git/config`
>
>     [remote "origin"]
>         url = git@github.com:jiaxianhua/jiaxianhua.github.io.git
>         url = ssh://551a7df75973ca0c1d00012a@markdown-jiaxianhua.rhcloud.com/~/git/ markdown.git/
>         fetch = +refs/heads/*:refs/remotes/origin/*
>
> `$ git pull`
> 
>     warning: no common commits
>     remote: Counting objects: 3, done.
>     remote: Total 3 (delta 0), reused 0 (delta 0), pack-reused 0
>     Unpacking objects: 100% (3/3), done.
>     From github.com:jiaxianhua/jiaxianhua.github.io
>      + f27c775...1290d2a master     ->        origin/master  (forced update)
>      Auto-merging README.md
>      CONFLICT (add/add): Merge conflict in README.md
>      Automatic merge failed; fix conflicts and then commit the result.
>      GnuHuadeMacBook-Air:markdown gnuhua$ git status
>      On branch master
>      Your branch and 'origin/master' have diverged,
>      and have 4 and 1 different commit each, respectively.
>         (use "git pull" to merge the remote branch into yours)
>     
>     You have unmerged paths.
>     (fix conflicts and run "git commit")
>     
>     Unmerged paths:
>     (use "git add <file>..." to mark resolution)
>     
>     **both added:      README.md**
>     
>     no changes added to commit (use "git add" and/or "git commit -a")
>     
>   `$ git diff`
>
>   `$ vim README.md`
>
>   `$ git status`
>
>     On branch master
>     Your branch and 'origin/master' have diverged,
>     and have 4 and 1 different commit each, respectively.
>       (use "git pull" to merge the remote branch into yours)
>     
>     You have unmerged paths.
>       (fix conflicts and run "git commit")
>   
>     Unmerged paths:
>       (use "git add >file>..." to mark resolution)
> 
>       both added:      README.md
>     
>     no changes added to commit (use "git add" and/or "git commit -a")
>
>   `$ git commit -am "Merge README"`
>
>     [master 89190bc] Merge README
>
>   `$ git status`
>
>     On branch master
>     Your branch is ahead of 'origin/master' by 5 commits.
>      (use "git push" to publish your local commits)
>
>     nothing to commit, working directory clean
>
>   `$ git push`
>
>     Counting objects: 124, done.
>     Delta compression using up to 4 threads.
>     Compressing objects: 100% (109/109), done.
>     Writing objects: 100% (122/122), 207.62 KiB | 0 bytes/s, done.
>     Total 122 (delta 13), reused 19 (delta 0)
>     To git@github.com:jiaxianhua/jiaxianhua.github.io.git
>        1290d2a..89190bc  master -> master
>        Counting objects: 8, done.
>        Delta compression using up to 4 threads.
>        Compressing objects: 100% (4/4), done.
>        Writing objects: 100% (6/6), 528 bytes | 0 bytes/s, done.
>        Total 6 (delta 2), reused 0 (delta 0)
>        remote: Stopping jekyll cart
>        remote: Sending SIGTERM to jekyll:450149 ...
>        remote: Building git ref 'master', commit 89190bc
>        remote: Preparing build for deployment
>        remote: Deployment id is 63930df6
>        remote: Activating deployment
>        remote: Starting jekyll cart
>        remote: Executing bundle install
>        remote: Fetching gem metadata from https://rubygems.org/.........
>        remote: Resolving dependencies...
>        remote: Using blankslate (2.1.2.4)
>        remote: Using hitimes (1.2.2)
>        remote: Using timers (4.0.1)
>        remote: Using celluloid (0.16.0)
>        remote: Using fast-stemmer (1.0.2)
>        remote: Using classifier-reborn (2.0.3)
>        remote: Using coffee-script-source (1.9.1)
>        remote: Using execjs (2.4.0)
>        remote: Using coffee-script (2.3.0)
>        remote: Using colorator (0.1)
>        remote: Using ffi (1.9.8)
>        remote: Using jekyll-coffeescript (1.0.1)
>        remote: Using jekyll-gist (1.2.1)
>        remote: Using jekyll-paginate (1.1.0)
>        remote: Using sass (3.4.13)
>        remote: Using jekyll-sass-converter (1.3.0)
>        remote: Using rb-fsevent (0.9.4)
>        remote: Using rb-inotify (0.9.5)
>        remote: Using listen (2.10.0)
>        remote: Using jekyll-watch (1.2.1)
>        remote: Using kramdown (1.6.0)
>        remote: Using liquid (2.6.2)
>        remote: Using mercenary (0.3.5)
>        remote: Using posix-spawn (0.3.10)
>        remote: Using yajl-ruby (1.2.1)
>        remote: Using pygments.rb (0.6.3)
>        remote: Using redcarpet (3.2.2)
>        remote: Using safe_yaml (1.0.4)
>        remote: Using parslet (1.5.0)
>        remote: Using toml (0.1.2)
>        remote: Using jekyll (2.5.3)
>        remote: Using json (1.8.2)
>        remote: Using bundler (1.3.5)
>        remote: Your bundle is complete!
>        remote: Use `bundle show [gemname]` to see where a bundled gem is installed.
>        remote: Starting Jekyll server
>        remote: Found 127.11.163.129:8080 listening port
>        remote: -------------------------
>        remote: Git Post-Receive Result: success
>        remote: Activation status: success
>        remote: Deployment completed with status: success
>        To ssh://551a7df75973ca0c1d00012a@markdown-jiaxianhua.rhcloud.com/~/git/markdown.git/
>           f27c775..89190bc  master -> master
