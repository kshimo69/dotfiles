return {
  'nvim-telescope/telescope.nvim',
  tag = '0.1.5',
  event = { "BufReadPre", "BufNewFile" },
  keys = {
    { "<leader>m", "<cmd>Telescope marks<cr>", desc = "Jump to Mark" },
  },
  dependencies = {
    'nvim-lua/plenary.nvim',
  },
  config = function()
    require("telescope").setup {
      defaults = {
        mappings = {
          i = {
            ['<C-u>'] = false,
            ['<C-d>'] = false,
          },
        },
      },
    }

    local builtin = require('telescope.builtin')

    -- Telescope live_grep in git root
    -- Function to find the git root directory based on the current buffer's path
    local function find_git_root()
      -- Use the current buffer's path as the starting point for the git search
      local current_file = vim.api.nvim_buf_get_name(0)
      local current_dir
      local cwd = vim.fn.getcwd()
      -- If the buffer is not associated with a file, return nil
      if current_file == '' then
        current_dir = cwd
      else
        -- Extract the directory from the current file's path
        current_dir = vim.fn.fnamemodify(current_file, ':h')
      end

      -- Find the Git root directory from the current file's path
      local git_root = vim.fn.systemlist('git -C ' .. vim.fn.escape(current_dir, ' ') .. ' rev-parse --show-toplevel')
      [1]
      if vim.v.shell_error ~= 0 then
        print 'Not a git repository. Searching on current working directory'
        return cwd
      end
      return git_root
    end
    -- Custom live_grep function to search in git root
    local function live_grep_git_root()
      local git_root = find_git_root()
      if git_root then
        builtin.live_grep {
          search_dirs = { git_root },
        }
      end
    end
    vim.api.nvim_create_user_command('LiveGrepGitRoot', live_grep_git_root, {})

    local function grep_string_git_root()
      local git_root = find_git_root()
      if git_root then
        builtin.grep_string {
          search_dirs = { git_root },
        }
      end
    end

    local function telescope_live_grep_open_files()
      builtin.live_grep {
        grep_open_files = true,
        prompt_title = 'Live Grep in Open Files',
      }
    end

    -- File
    vim.keymap.set('n', '<leader>f/', telescope_live_grep_open_files, { desc = 'search [/] in Open Files' })
    vim.keymap.set('n', '<C-p>', builtin.find_files, { desc = 'search [F]iles' })
    vim.keymap.set('n', '<leader>fg', live_grep_git_root, { desc = 'search by [G]rep on Git Root' })
    vim.keymap.set('n', '<C-g>', live_grep_git_root, { desc = 'search by [G]rep on Git Root' })
    vim.keymap.set('n', '<leader>fw', grep_string_git_root, { desc = 'search current [W]ord on Git Root' })
    -- vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'search [H]elp' })

    -- Vim
    vim.keymap.set('n', '<leader>fh', builtin.oldfiles, { desc = 'search file [H]istory' })
    vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'find existing [B]uffers' })
    vim.keymap.set('n', '<leader>fy', builtin.registers, { desc = 'search [Y]ank list' })
    vim.keymap.set('n', '<leader>fq', builtin.quickfixhistory, { desc = 'search [Q]uickfix history' })
    vim.keymap.set('n', '<leader>fr', builtin.resume, { desc = 'search [R]esume' })
    vim.keymap.set('n', '<leader>fm', builtin.marks, { desc = 'search [M]arks' })
    vim.keymap.set('n', '<leader>fc', builtin.commands, { desc = 'search [C]ommands' })

    -- LSP
  end
}
