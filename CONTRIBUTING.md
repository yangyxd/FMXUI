Contributing new code or documentation updates
----------------------------------------------
To contribute new code or documentation updates to FMXUI clone your own
fork instead of cloning the beta FMXUI repository, commit your work on topic
branches and make pull requests. In detail:

1. [Fork](https://help.github.com/en/github/getting-started-with-github/fork-a-repo) the project.

2. Clone your fork (`git clone https://github.com/<your-username>/FMXUI.git`).

3. Add an `upstream` remote (`git remote add upstream
   https://github.com/yangyxd/FMXUI.git`).

4. Get the latest changes from upstream (e.g. `git pull upstream beta`).

5. Create a new topic branch to contain your feature, change, or fix (`git
   checkout -b <topic-branch-name>`).

6. Make sure that your changes adhere to the current coding conventions used
   throughout the project - indentation, accurate comments, etc.
   Do not make mass whitespace, copyright or linebreak changes to files. 

7. Commit your changes to your topic branch.

8. Push your topic branch up to your fork (`git push origin
   <topic-branch-name>`).

9. [Open a Pull Request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests) with a
    clear title and description. 

10. Being an FMXUI contributor :)

If you don't have the Git client (`git`), get it from: https://git-scm.com/