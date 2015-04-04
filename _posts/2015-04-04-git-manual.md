---
layout: post
title: "Git Manual"
description: ""
category: 
tags: [git]
---
{% include JB/setup %}

## Create  
---

Clone an existing repository  

>    `git clone ssh://user@domain.tld/repo.git`    

Clone an existing repository and all its sub-modules recursively  

>    `git clone --recursive ssh://user@domain.tld/repo.git`    

Create a new local repository  

>    `git init`    

## Local Changes  
---

List changed files in your working directory  

>    `git status`    

List changes to tracked files  

>    `git diff`    

Add all current changes to the next commit  

>    `git add .`    

Add some changes to the next commit  

>    `git add -p <file>`    

Commit all local changes in tracked files  

>    `git commit -a`    

Commit previously staged changes  

>    `git commit`    

Change the last commit  

>    `git commit --amend`    

Note: You shouldn't amend published commits!  

## Commit History  
---

Show all commits  

>    `git log`    

Show changes over time for a specific file  

>    `git log -p <file>`    

Show changes over time for a specific committer  

>    `git log --author=<committer name>`    


Note: `<committer name>` is a pattern, so Ed will match Edward Smith. Quotes are optional if the pattern doesn't contain spaces.  
Who changed what and when in file  

>    `git blame <file>`    

Store changes temporarily  

>    `git stash`    

Remove and apply stashed changes  

>    `git stash pop`    

## Branches & Tags  
---

List all existing branches  

>    `git branch`    

Switch HEAD branch  

>    `git checkout <branch>`    

Create a new branch based on your current HEAD  

>    `git branch <new-branch>`    

Create a new tracking branch based on a remote branch  

>    `git branch --track <new-branch> <remote-branch>`    

Delete a local branch  

>    `git branch -d <branch>`    

Delete a remote branch  

>    `git push origin --delete <branch>`    

Tag the current commit  

>    `git tag <tag-name>`    

## Update & Publish  
---

List all currently configured remotes  

>    `git remote -v`    

Show information about a remote  

>    `git remote show <remote>`    

Add new remote repository  

>    `git remote add <remote> <url>`    

Download all changes from remote, but don't merge into HEAD  

>    `git fetch <remote>`    

Download all changes from remote, but don't merge into HEAD and clean up deleted branches from origin  

>    `git fetch -p <remote>`    

Download changes and directly merge into HEAD  

>    `git pull <remote> <branch>`    

Publish local changes on a remote  

>    `git push <remote> <branch>`    

Track a remote repository  

>    `git remote add --track <remote-branch> <remote> <url>`    

Publish your tags  

>    `git push --tags`    

## Merge & Rebase  
---

Merge branch into your current HEAD  

>    `git merge <branch>`    

Rebase your current HEAD onto branch  

>    `git rebase <branch>`    

Note: You shouldn't rebase published commits!  
Abort a rebase  

>    `git rebase --abort`    

Continue a rebase after resolving conflicts  

>    `git rebase --continue`    

Resolve conflicts using your configured merge tool  

>    `git mergetool`    

Manually resolve conflicts using your editor and mark file as resolved  

>    `git add <resolved-file>`    


>    `git rm <resolved-file>`    

## Undo  
---

Discard all local changes in your working directory  

>    `git reset --hard HEAD`    

Discard local changes in a specific file  

>    `git checkout HEAD <file>`    

Revert a commit by providing a new commit with contrary changes  

>    `git revert <commit>`    

Restore a specific file from a previous commit  

>    `git checkout <commit> <file>`    

Reset your HEAD pointer to a previous commit  
Discarding local changes:  

>    `git reset --hard <commit>`    

Preserving all changes as unstaged changes:  

>    `git reset <commit>`    

Preserving uncommitted local changes:  

>    `git reset --keep <commit>`    


# Notes  
---

Based on the cheat sheet from [Tower.app][]. The original can be found [here][].  

[Tower.app]: www.git-tower.com
[here]: www.git-tower.com/blog/git-cheat-sheet/
