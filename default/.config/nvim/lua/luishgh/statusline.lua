local devicons = require "nvim-web-devicons"
local lsp_status = require "lsp-status"

if (not devicons.has_loaded()) then
    devicons.setup()
end

local M = {}

-- Mode Prompt Table
local mode_map =
    setmetatable(
    {
        ["n"] = "NORMAL",
        ["no"] = "N·Operator Pending",
        ["v"] = "VISUAL",
        ["V"] = "V·Line",
        ["^V"] = "V·Block",
        ["s"] = "Select",
        ["S"] = "S·Line",
        ["^S"] = "S·Block",
        ["i"] = "INSERT",
        ["ic"] = "INSERT",
        ["ix"] = "INSERT",
        ["R"] = "Replace",
        ["Rv"] = "V·Replace",
        ["c"] = "COMMAND",
        ["cv"] = "Vim Ex",
        ["ce"] = "Ex",
        ["r"] = "Prompt",
        ["rm"] = "More",
        ["r?"] = "Confirm",
        ["!"] = "Shell",
        ["t"] = "TERMINAL"
    },
    {
        -- fix weird issues
        __index = function(_, _)
            return "V·Block"
        end
    }
)

local function mode()
    local m = vim.api.nvim_get_mode().mode
    if mode_map[m] == nil then
        return m
    end
    return mode_map[m]
end

local function filetype()
    local filename = vim.api.nvim_buf_get_name(0)
    local icon = devicons.get_icon(filename, string.match(filename, "%a+$"), {default = true})
    return icon
end

local function lsp_diagnostics()
    if next(vim.lsp.buf_get_clients()) == nil then -- Checks if current buffer has a lsp server attached to it
        return ""
    else
        return string.format(":%s :%s :%s", lsp_status.diagnostics().errors, lsp_status.diagnostics().warnings, lsp_status.diagnostics().hints)
    end
end

function M.statusline()
    local statusline = ""
    -- Component: Mode
    statusline = statusline .. mode() .. " "

    -- Component: Filetype
    statusline = statusline .. filetype() .. " "

    -- Component: Working Directory
    statusline = statusline .. "%f "

    -- Component: Modified Flag
    statusline = statusline .. "%m "

    -- Alignment to left
    statusline = statusline .. "%="

    -- Component: LSP Diagnostics
    statusline = statusline .. lsp_diagnostics() .. " "

    -- Component: Row & Column
    -- statusline = statusline .. "%l,%c "

    return statusline
end

return M
