# Getting the app online (about 10 minutes, one time)

When you're done, the app will live at `https://YOUR-USERNAME.github.io/seeing-research-design/`. Every time you change `app/app.R` on GitHub, the website rebuilds itself automatically.

## 1. Create the repository
1. Sign in at github.com. If you don't have an account, create one; the username becomes part of the link.
2. Click **+** (top right) → **New repository**.
3. Name it **`seeing-research-design`**, choose **Public** (free GitHub Pages requires a public repository), and leave every "Add a README / .gitignore / license" box **unticked**. Click **Create repository**.

## 2. Upload the files
Use **GitHub Desktop** (desktop.github.com). It's the easiest way to upload, and later it's how you'll push edits.

1. In GitHub Desktop: **File → Add local repository…** and choose this unzipped `seeing-research-design` folder. It's already set up as a git repository with one commit.
2. Click **Publish repository**. Untick "Keep this code private", and make sure the name is `seeing-research-design`. If GitHub Desktop says the repository already exists, use **Repository → Repository settings → Remote** and paste `https://github.com/YOUR-USERNAME/seeing-research-design.git`, then click **Push origin**.

<details><summary>Prefer the command line?</summary>

```bash
cd seeing-research-design
git remote add origin https://github.com/YOUR-USERNAME/seeing-research-design.git
git push -u origin main
```
</details>

<details><summary>No GitHub Desktop? Uploading through the website</summary>

On the repository page, click **uploading an existing file** and drag in `README.md`, `SETUP.md`, `.gitignore` and the `app` folder. Then create the workflow by hand. Click **Add file → Create new file**, type the name `.github/workflows/deploy.yml` (the slashes create the folders), paste in the contents of that file from this folder, and commit. The folder is hidden on macOS; press Cmd+Shift+. in Finder to see it.
</details>

## 3. Turn on GitHub Pages
1. In the repository: **Settings → Pages**.
2. Under **Build and deployment → Source**, choose **GitHub Actions**. Do not choose "Deploy from a branch."

## 4. Run the first build
1. Open the **Actions** tab. A run called **Build and publish app** should start, or may already have started. If it failed because Pages wasn't switched on yet, click it and choose **Re-run all jobs**. You can also click **Build and publish app → Run workflow**.
2. The first build takes about 5–10 minutes because it installs R. Later builds are faster.
3. When it shows a green tick, open `https://YOUR-USERNAME.github.io/seeing-research-design/`.

Finally, replace `YOUR-USERNAME` in `README.md` with your real username. You can edit it on github.com with the pencil icon, and that edit triggers a rebuild too.

## Making changes later
Edit `app/app.R` (on github.com, or locally and then push with GitHub Desktop). The site republishes itself within a few minutes. Test locally first with `shiny::runApp("app")`.

## Adding it to Google Sites
- **Recommended:** Insert a **Button** with the app link, so it opens in a full browser tab.
- **Optional:** **Insert → Embed → By URL** with the same link. Check that it loads in Chrome *and* Safari before relying on it, since in-browser apps don't always run inside embedded frames.

## If something goes wrong
- **Build shows a red ✕:** click it and open the failed step. A failed "Install R packages" step is usually a temporary hiccup at the package server; click **Re-run all jobs**.
- **404 at the link:** Pages isn't set to *GitHub Actions* (step 3), or the first build hasn't finished yet.
- **Stuck on the loading spinner:** the first load downloads R into the browser, so give it 30 seconds on slow campus Wi-Fi. Older iPads and phones may struggle; laptops work best.
