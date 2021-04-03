-- LuisHGH's custom telescope functions

local pickers = require("telescope.pickers")
local utils = require("telescope.utils")
local make_entry = require("telescope.make_entry")
local finders = require("telescope.finders")

local conf = require("telescope.config").values

local M = {}

-- ------------------- Pickers ----------------------------------------------

-- a little fork from the builtin git_files picker to use with worktrees from bare repos
-- unfortunately, this only works if cwd is the root of the worktree and there seems to be no way of guaranteeing it
-- required params:
--     - git_dir
M.git_bare_files = function(opts)
    if (not opts.git_dir) then
        error("git_dir is required")
    end

    local _, ret, _ = utils.get_os_command_output({"git", "rev-parse", "--is-inside-git-dir"}, opts.git_dir)
    if ret ~= 0 then
        print('suus')
        error(opts.git_dir .. " is not a git bare repository")
    end

    opts.git_dir = vim.fn.expand(opts.git_dir)

    local show_untracked = utils.get_default(opts.show_untracked, true)
    local recurse_submodules = utils.get_default(opts.recurse_submodules, false)
    if show_untracked and recurse_submodules then
        error("Git does not suppurt both --others and --recurse-submodules")
    end

    -- By creating the entry maker after the cwd options,
    -- we ensure the maker uses the cwd options when being created.
    opts.entry_maker = opts.entry_maker or make_entry.gen_from_file(opts)

    pickers.new(
        opts,
        {
            prompt_title = "Git Worktree Files",
            finder = finders.new_oneshot_job(
                vim.tbl_flatten(
                    {
                        "git",
                        string.format("--git-dir=%s", opts.git_dir),
                        "ls-files",
                        "--exclude-standard",
                        "--cached",
                        show_untracked and "--others" or nil,
                        recurse_submodules and "--recurse-submodules" or nil
                    }
                ),
                opts
            ),
            previewer = conf.file_previewer(opts),
            sorter = conf.file_sorter(opts)
        }
    ):find()
end

local function apply_checks(mod)
    for k, v in pairs(mod) do
        mod[k] = function(opts)
            opts = opts or {}

            v(opts)
        end
    end

    return mod
end

-- ------------------- Configs ----------------------------------------------

-- custom find_files config to fuzzy find my neovim dotfiles
M.vimrc = function()
    require("telescope.builtin").find_files {
        prompt_title = "VimRC",
        cwd = "$HOME/.config/nvim",
        file_ignore_patterns = {
            -- dirs to be ignored by telescope
            "autoload/*",
            "bundle/*",
            "cache/*"
        }
    }
end

-- custom git_bare_files to fuzzy find my dotfiles
M.dotfiles = function()
    M.git_bare_files {
        prompt_title = "Dotfiles",
        git_dir = "$HOME/Documents/dotfiles",
        show_untracked = false,
        cwd = "$HOME"
    }
end

return apply_checks(M)
