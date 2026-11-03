open! Core

(* This file contains a variant type for enumerating Codicons icons. *)

(* WARNING: Depending on this lib will cause every Codicons icon to be included in your
   build output, which will likely harm performance. Instead use
   [Bonsai_web_icon_codicons] directly. *)

module Private = struct
  type t =
    | Account
    | Activate_breakpoints
    | Add
    | Add_compact
    | Add_small
    | Agent
    | Agent_compact
    | Archive
    | Arrow_both
    | Arrow_circle_down
    | Arrow_circle_left
    | Arrow_circle_right
    | Arrow_circle_up
    | Arrow_circle_up_sparkle
    | Arrow_down
    | Arrow_left
    | Arrow_right
    | Arrow_small_down
    | Arrow_small_left
    | Arrow_small_right
    | Arrow_small_up
    | Arrow_swap
    | Arrow_up
    | Arrow_up_compact
    | Ask
    | Ask_compact
    | Attach
    | Attach_compact
    | Azure
    | Azure_devops
    | Beaker
    | Beaker_compact
    | Beaker_stop
    | Bell
    | Bell_dot
    | Bell_slash
    | Bell_slash_dot
    | Blank
    | Bold
    | Book
    | Bookmark
    | Bracket_dot
    | Bracket_error
    | Briefcase
    | Broadcast
    | Browser
    | Bug
    | Build
    | Calendar
    | Call_incoming
    | Call_outgoing
    | Case_sensitive
    | Chat_export
    | Chat_import
    | Chat_sparkle
    | Chat_sparkle_error
    | Chat_sparkle_warning
    | Check
    | Check_all
    | Check_compact
    | Checklist
    | Checklist_compact
    | Chevron_down
    | Chevron_down_compact
    | Chevron_left
    | Chevron_left_compact
    | Chevron_right
    | Chevron_right_compact
    | Chevron_up
    | Chevron_up_compact
    | Chip
    | Chrome_close
    | Chrome_maximize
    | Chrome_minimize
    | Chrome_restore
    | Circle
    | Circle_filled
    | Circle_filled_compact
    | Circle_large
    | Circle_large_filled
    | Circle_slash
    | Circle_slash_compact
    | Circle_small
    | Circle_small_filled
    | Circle_small_filled_compact
    | Circuit_board
    | Claude
    | Clear_all
    | Clippy
    | Clockface
    | Close
    | Close_all
    | Close_compact
    | Close_small
    | Cloud
    | Cloud_compact
    | Cloud_download
    | Cloud_download_compact
    | Cloud_small
    | Cloud_upload
    | Cloud_upload_compact
    | Code
    | Code_oss
    | Code_review
    | Coffee
    | Collapse_all
    | Collapse_all_compact
    | Collection
    | Collection_small
    | Color_mode
    | Combine
    | Comment
    | Comment_compact
    | Comment_discussion
    | Comment_discussion_quote
    | Comment_discussion_sparkle
    | Comment_draft
    | Comment_unresolved
    | Comment_unresolved_compact
    | Compass
    | Compass_active
    | Compass_dot
    | Copilot
    | Copilot_blocked
    | Copilot_compact
    | Copilot_error
    | Copilot_in_progress
    | Copilot_large
    | Copilot_not_connected
    | Copilot_snooze
    | Copilot_success
    | Copilot_unavailable
    | Copilot_warning
    | Copilot_warning_large
    | Copy
    | Coverage
    | Credit_card
    | Cursor
    | Dash
    | Dashboard
    | Database
    | Debug
    | Debug_all
    | Debug_alt
    | Debug_alt_small
    | Debug_breakpoint_conditional
    | Debug_breakpoint_conditional_unverified
    | Debug_breakpoint_data
    | Debug_breakpoint_data_unverified
    | Debug_breakpoint_function
    | Debug_breakpoint_function_unverified
    | Debug_breakpoint_log
    | Debug_breakpoint_log_unverified
    | Debug_breakpoint_unsupported
    | Debug_connected
    | Debug_connected_compact
    | Debug_console
    | Debug_continue
    | Debug_continue_small
    | Debug_coverage
    | Debug_disconnect
    | Debug_disconnect_compact
    | Debug_line_by_line
    | Debug_pause
    | Debug_rerun
    | Debug_restart
    | Debug_restart_frame
    | Debug_reverse_continue
    | Debug_stackframe
    | Debug_stackframe_active
    | Debug_start
    | Debug_step_back
    | Debug_step_into
    | Debug_step_out
    | Debug_step_over
    | Debug_stop
    | Desktop_download
    | Developer_tools
    | Device_camera
    | Device_camera_video
    | Device_mobile
    | Diff
    | Diff_added
    | Diff_ignored
    | Diff_modified
    | Diff_multiple
    | Diff_removed
    | Diff_renamed
    | Diff_single
    | Discard
    | Download
    | Edit
    | Edit_code
    | Edit_compact
    | Edit_session
    | Edit_sparkle
    | Editor_layout
    | Ellipsis
    | Empty_window
    | Eraser
    | Error
    | Error_compact
    | Error_small
    | Exclude
    | Expand_all
    | Export
    | Extensions
    | Extensions_large
    | Eye
    | Eye_closed
    | Feedback
    | File
    | File_binary
    | File_code
    | File_media
    | File_media_compact
    | File_pdf
    | File_submodule
    | File_symlink_directory
    | File_symlink_file
    | File_text
    | File_zip
    | Files
    | Filter
    | Filter_filled
    | Fish1_happy
    | Fish1_neutral
    | Fish1_sad
    | Fish1_very_sad
    | Fish2_happy
    | Fish2_neutral
    | Fish2_sad
    | Fish2_very_sad
    | Fish3_happy
    | Fish3_neutral
    | Fish3_sad
    | Fish3_very_sad
    | Fish4_happy
    | Fish4_neutral
    | Fish4_sad
    | Fish4_very_sad
    | Flag
    | Flame
    | Fold
    | Fold_down
    | Fold_up
    | Folder
    | Folder_active
    | Folder_compact
    | Folder_library
    | Folder_opened
    | Folder_opened_compact
    | Forward
    | Game
    | Gear
    | Gear_compact
    | Gift
    | Gist
    | Gist_secret
    | Git_branch
    | Git_branch_changes
    | Git_branch_compact
    | Git_branch_conflicts
    | Git_branch_staged_changes
    | Git_commit
    | Git_compare
    | Git_fetch
    | Git_merge
    | Git_pull_request
    | Git_pull_request_closed
    | Git_pull_request_comment
    | Git_pull_request_create
    | Git_pull_request_done
    | Git_pull_request_draft
    | Git_pull_request_error
    | Git_pull_request_go_to_changes
    | Git_pull_request_new_changes
    | Git_stash
    | Git_stash_apply
    | Git_stash_pop
    | Github
    | Github_action
    | Github_alt
    | Github_inverted
    | Github_project
    | Globe
    | Go_to_editing_session
    | Go_to_file
    | Go_to_search
    | Google_gemini
    | Grabber
    | Graph
    | Graph_left
    | Graph_line
    | Graph_scatter
    | Gripper
    | Group_by_ref_type
    | Heart
    | Heart_filled
    | History
    | Home
    | Horizontal_rule
    | Hubot
    | Important
    | Important_compact
    | Inbox
    | Indent
    | Index_zero
    | Info
    | Insert
    | Inspect
    | Issue_draft
    | Issue_reopened
    | Issues
    | Italic
    | Jersey
    | Json
    | Kebab_vertical
    | Key
    | Keyboard_tab
    | Keyboard_tab_above
    | Keyboard_tab_below
    | Kimi
    | Law
    | Layers
    | Layers_active
    | Layers_dot
    | Layout
    | Layout_activitybar_left
    | Layout_activitybar_right
    | Layout_centered
    | Layout_menubar
    | Layout_panel
    | Layout_panel_center
    | Layout_panel_dock
    | Layout_panel_justify
    | Layout_panel_left
    | Layout_panel_off
    | Layout_panel_right
    | Layout_sidebar_left
    | Layout_sidebar_left_dock
    | Layout_sidebar_left_off
    | Layout_sidebar_right
    | Layout_sidebar_right_dock
    | Layout_sidebar_right_off
    | Layout_statusbar
    | Library
    | Library_compact
    | Lightbulb
    | Lightbulb_autofix
    | Lightbulb_compact
    | Lightbulb_empty
    | Lightbulb_sparkle
    | Link
    | Link_external
    | List_filter
    | List_flat
    | List_ordered
    | List_selection
    | List_tree
    | List_unordered
    | Live_share
    | Loading
    | Loading_compact
    | Location
    | Lock
    | Lock_small
    | Magnet
    | Mail
    | Mail_read
    | Map
    | Map_filled
    | Map_vertical
    | Map_vertical_filled
    | Markdown
    | Mcp
    | Megaphone
    | Mention
    | Menu
    | Merge
    | Merge_into
    | Mic
    | Mic_compact
    | Mic_download
    | Mic_download_compact
    | Mic_filled
    | Microsoft
    | Milestone
    | Mirror
    | Mortar_board
    | Move
    | Multiple_windows
    | Music
    | Mute
    | New_collection
    | New_file
    | New_folder
    | New_session
    | Newline
    | No_newline
    | Note
    | Notebook
    | Notebook_template
    | Octoface
    | Open_in_product
    | Open_in_window
    | Open_preview
    | Openai
    | Organization
    | Output
    | Package
    | Paintcan
    | Pass
    | Pass_compact
    | Pass_filled
    | Pass_filled_compact
    | Percentage
    | Person
    | Person_add
    | Person_voice
    | Person_voice_compact
    | Person_voice_filled
    | Person_voice_filled_compact
    | Piano
    | Pie_chart
    | Pin
    | Pinned
    | Pinned_dirty
    | Play
    | Play_circle
    | Plug
    | Preserve_case
    | Preview
    | Primitive_square
    | Project
    | Project_compact
    | Pulse
    | Python
    | Question
    | Quote
    | Quotes
    | Radio_tower
    | Reactions
    | Record
    | Record_keys
    | Record_keys_compact
    | Record_small
    | Redo
    | References
    | Refresh
    | Refresh_compact
    | Regex
    | Remote
    | Remote_compact
    | Remote_explorer
    | Remove
    | Remove_small
    | Rename
    | Replace
    | Replace_all
    | Reply
    | Repo
    | Repo_clone
    | Repo_compact
    | Repo_fetch
    | Repo_force_push
    | Repo_forked
    | Repo_forked_compact
    | Repo_pinned
    | Repo_pull
    | Repo_push
    | Repo_selected
    | Report
    | Request_changes
    | Right_panel_hide
    | Right_panel_show
    | Robot
    | Rocket
    | Rocket_compact
    | Root_folder
    | Root_folder_opened
    | Rss
    | Ruby
    | Run_above
    | Run_all
    | Run_all_coverage
    | Run_below
    | Run_compact
    | Run_coverage
    | Run_errors
    | Run_with_deps
    | Save
    | Save_all
    | Save_as
    | Screen_cut
    | Screen_full
    | Screen_normal
    | Search
    | Search_compact
    | Search_fuzzy
    | Search_large
    | Search_sparkle
    | Search_stop
    | Send
    | Send_to_remote_agent
    | Server
    | Server_environment
    | Server_process
    | Session_in_progress
    | Session_in_progress_compact
    | Settings
    | Settings_compact
    | Settings_gear
    | Share
    | Share_window
    | Shield
    | Shield_compact
    | Sign_in
    | Sign_out
    | Skip
    | Smiley
    | Snake
    | Sort_precedence
    | Source_control
    | Sparkle
    | Sparkle_compact
    | Sparkle_filled
    | Split_horizontal
    | Split_vertical
    | Squirrel
    | Star_empty
    | Star_full
    | Star_half
    | Stop_circle
    | Strikethrough
    | Surround_with
    | Symbol_array
    | Symbol_boolean
    | Symbol_class
    | Symbol_color
    | Symbol_color_compact
    | Symbol_constant
    | Symbol_enum
    | Symbol_enum_member
    | Symbol_event
    | Symbol_field
    | Symbol_file
    | Symbol_interface
    | Symbol_key
    | Symbol_keyword
    | Symbol_method
    | Symbol_method_arrow
    | Symbol_misc
    | Symbol_namespace
    | Symbol_numeric
    | Symbol_operator
    | Symbol_parameter
    | Symbol_property
    | Symbol_ruler
    | Symbol_snippet
    | Symbol_string
    | Symbol_structure
    | Symbol_variable
    | Sync
    | Sync_compact
    | Sync_ignored
    | Table
    | Tag
    | Target
    | Tasklist
    | Telescope
    | Terminal
    | Terminal_bash
    | Terminal_cmd
    | Terminal_compact
    | Terminal_debian
    | Terminal_git_bash
    | Terminal_linux
    | Terminal_powershell
    | Terminal_secure
    | Terminal_tmux
    | Terminal_ubuntu
    | Text_size
    | Thinking
    | Three_bars
    | Thumbsdown
    | Thumbsdown_filled
    | Thumbsup
    | Thumbsup_filled
    | Tools
    | Trash
    | Triangle_down
    | Triangle_left
    | Triangle_right
    | Triangle_up
    | Twitter
    | Type_hierarchy
    | Type_hierarchy_sub
    | Type_hierarchy_super
    | Unarchive
    | Unfold
    | Ungroup_by_ref_type
    | Unlock
    | Unmute
    | Unpin
    | Unverified
    | Variable_group
    | Verified
    | Verified_filled
    | Versions
    | Vm
    | Vm_active
    | Vm_compact
    | Vm_connect
    | Vm_outline
    | Vm_pending
    | Vm_running
    | Vm_small
    | Voice_mode
    | Voice_mode_compact
    | Voice_mode_download
    | Voice_mode_download_compact
    | Vr
    | Vscode
    | Vscode_insiders
    | Vscode_insiders_outline
    | Vscode_outline
    | Wand
    | Warning
    | Warning_compact
    | Watch
    | Whitespace
    | Whole_word
    | Window
    | Window_active
    | Window_compact
    | Word_wrap
    | Workspace_trusted
    | Workspace_unknown
    | Workspace_untrusted
    | Worktree
    | Worktree_compact
    | Worktree_small
    | Xai
    | Zoom_in
    | Zoom_out
  [@@deriving enumerate]

  let to_icon = function
    | Account -> Bonsai_web_icon_codicons.account
    | Activate_breakpoints -> Bonsai_web_icon_codicons.activate_breakpoints
    | Add -> Bonsai_web_icon_codicons.add
    | Add_compact -> Bonsai_web_icon_codicons.add_compact
    | Add_small -> Bonsai_web_icon_codicons.add_small
    | Agent -> Bonsai_web_icon_codicons.agent
    | Agent_compact -> Bonsai_web_icon_codicons.agent_compact
    | Archive -> Bonsai_web_icon_codicons.archive
    | Arrow_both -> Bonsai_web_icon_codicons.arrow_both
    | Arrow_circle_down -> Bonsai_web_icon_codicons.arrow_circle_down
    | Arrow_circle_left -> Bonsai_web_icon_codicons.arrow_circle_left
    | Arrow_circle_right -> Bonsai_web_icon_codicons.arrow_circle_right
    | Arrow_circle_up -> Bonsai_web_icon_codicons.arrow_circle_up
    | Arrow_circle_up_sparkle -> Bonsai_web_icon_codicons.arrow_circle_up_sparkle
    | Arrow_down -> Bonsai_web_icon_codicons.arrow_down
    | Arrow_left -> Bonsai_web_icon_codicons.arrow_left
    | Arrow_right -> Bonsai_web_icon_codicons.arrow_right
    | Arrow_small_down -> Bonsai_web_icon_codicons.arrow_small_down
    | Arrow_small_left -> Bonsai_web_icon_codicons.arrow_small_left
    | Arrow_small_right -> Bonsai_web_icon_codicons.arrow_small_right
    | Arrow_small_up -> Bonsai_web_icon_codicons.arrow_small_up
    | Arrow_swap -> Bonsai_web_icon_codicons.arrow_swap
    | Arrow_up -> Bonsai_web_icon_codicons.arrow_up
    | Arrow_up_compact -> Bonsai_web_icon_codicons.arrow_up_compact
    | Ask -> Bonsai_web_icon_codicons.ask
    | Ask_compact -> Bonsai_web_icon_codicons.ask_compact
    | Attach -> Bonsai_web_icon_codicons.attach
    | Attach_compact -> Bonsai_web_icon_codicons.attach_compact
    | Azure -> Bonsai_web_icon_codicons.azure
    | Azure_devops -> Bonsai_web_icon_codicons.azure_devops
    | Beaker -> Bonsai_web_icon_codicons.beaker
    | Beaker_compact -> Bonsai_web_icon_codicons.beaker_compact
    | Beaker_stop -> Bonsai_web_icon_codicons.beaker_stop
    | Bell -> Bonsai_web_icon_codicons.bell
    | Bell_dot -> Bonsai_web_icon_codicons.bell_dot
    | Bell_slash -> Bonsai_web_icon_codicons.bell_slash
    | Bell_slash_dot -> Bonsai_web_icon_codicons.bell_slash_dot
    | Blank -> Bonsai_web_icon_codicons.blank
    | Bold -> Bonsai_web_icon_codicons.bold
    | Book -> Bonsai_web_icon_codicons.book
    | Bookmark -> Bonsai_web_icon_codicons.bookmark
    | Bracket_dot -> Bonsai_web_icon_codicons.bracket_dot
    | Bracket_error -> Bonsai_web_icon_codicons.bracket_error
    | Briefcase -> Bonsai_web_icon_codicons.briefcase
    | Broadcast -> Bonsai_web_icon_codicons.broadcast
    | Browser -> Bonsai_web_icon_codicons.browser
    | Bug -> Bonsai_web_icon_codicons.bug
    | Build -> Bonsai_web_icon_codicons.build
    | Calendar -> Bonsai_web_icon_codicons.calendar
    | Call_incoming -> Bonsai_web_icon_codicons.call_incoming
    | Call_outgoing -> Bonsai_web_icon_codicons.call_outgoing
    | Case_sensitive -> Bonsai_web_icon_codicons.case_sensitive
    | Chat_export -> Bonsai_web_icon_codicons.chat_export
    | Chat_import -> Bonsai_web_icon_codicons.chat_import
    | Chat_sparkle -> Bonsai_web_icon_codicons.chat_sparkle
    | Chat_sparkle_error -> Bonsai_web_icon_codicons.chat_sparkle_error
    | Chat_sparkle_warning -> Bonsai_web_icon_codicons.chat_sparkle_warning
    | Check -> Bonsai_web_icon_codicons.check
    | Check_all -> Bonsai_web_icon_codicons.check_all
    | Check_compact -> Bonsai_web_icon_codicons.check_compact
    | Checklist -> Bonsai_web_icon_codicons.checklist
    | Checklist_compact -> Bonsai_web_icon_codicons.checklist_compact
    | Chevron_down -> Bonsai_web_icon_codicons.chevron_down
    | Chevron_down_compact -> Bonsai_web_icon_codicons.chevron_down_compact
    | Chevron_left -> Bonsai_web_icon_codicons.chevron_left
    | Chevron_left_compact -> Bonsai_web_icon_codicons.chevron_left_compact
    | Chevron_right -> Bonsai_web_icon_codicons.chevron_right
    | Chevron_right_compact -> Bonsai_web_icon_codicons.chevron_right_compact
    | Chevron_up -> Bonsai_web_icon_codicons.chevron_up
    | Chevron_up_compact -> Bonsai_web_icon_codicons.chevron_up_compact
    | Chip -> Bonsai_web_icon_codicons.chip
    | Chrome_close -> Bonsai_web_icon_codicons.chrome_close
    | Chrome_maximize -> Bonsai_web_icon_codicons.chrome_maximize
    | Chrome_minimize -> Bonsai_web_icon_codicons.chrome_minimize
    | Chrome_restore -> Bonsai_web_icon_codicons.chrome_restore
    | Circle -> Bonsai_web_icon_codicons.circle
    | Circle_filled -> Bonsai_web_icon_codicons.circle_filled
    | Circle_filled_compact -> Bonsai_web_icon_codicons.circle_filled_compact
    | Circle_large -> Bonsai_web_icon_codicons.circle_large
    | Circle_large_filled -> Bonsai_web_icon_codicons.circle_large_filled
    | Circle_slash -> Bonsai_web_icon_codicons.circle_slash
    | Circle_slash_compact -> Bonsai_web_icon_codicons.circle_slash_compact
    | Circle_small -> Bonsai_web_icon_codicons.circle_small
    | Circle_small_filled -> Bonsai_web_icon_codicons.circle_small_filled
    | Circle_small_filled_compact -> Bonsai_web_icon_codicons.circle_small_filled_compact
    | Circuit_board -> Bonsai_web_icon_codicons.circuit_board
    | Claude -> Bonsai_web_icon_codicons.claude
    | Clear_all -> Bonsai_web_icon_codicons.clear_all
    | Clippy -> Bonsai_web_icon_codicons.clippy
    | Clockface -> Bonsai_web_icon_codicons.clockface
    | Close -> Bonsai_web_icon_codicons.close
    | Close_all -> Bonsai_web_icon_codicons.close_all
    | Close_compact -> Bonsai_web_icon_codicons.close_compact
    | Close_small -> Bonsai_web_icon_codicons.close_small
    | Cloud -> Bonsai_web_icon_codicons.cloud
    | Cloud_compact -> Bonsai_web_icon_codicons.cloud_compact
    | Cloud_download -> Bonsai_web_icon_codicons.cloud_download
    | Cloud_download_compact -> Bonsai_web_icon_codicons.cloud_download_compact
    | Cloud_small -> Bonsai_web_icon_codicons.cloud_small
    | Cloud_upload -> Bonsai_web_icon_codicons.cloud_upload
    | Cloud_upload_compact -> Bonsai_web_icon_codicons.cloud_upload_compact
    | Code -> Bonsai_web_icon_codicons.code
    | Code_oss -> Bonsai_web_icon_codicons.code_oss
    | Code_review -> Bonsai_web_icon_codicons.code_review
    | Coffee -> Bonsai_web_icon_codicons.coffee
    | Collapse_all -> Bonsai_web_icon_codicons.collapse_all
    | Collapse_all_compact -> Bonsai_web_icon_codicons.collapse_all_compact
    | Collection -> Bonsai_web_icon_codicons.collection
    | Collection_small -> Bonsai_web_icon_codicons.collection_small
    | Color_mode -> Bonsai_web_icon_codicons.color_mode
    | Combine -> Bonsai_web_icon_codicons.combine
    | Comment -> Bonsai_web_icon_codicons.comment
    | Comment_compact -> Bonsai_web_icon_codicons.comment_compact
    | Comment_discussion -> Bonsai_web_icon_codicons.comment_discussion
    | Comment_discussion_quote -> Bonsai_web_icon_codicons.comment_discussion_quote
    | Comment_discussion_sparkle -> Bonsai_web_icon_codicons.comment_discussion_sparkle
    | Comment_draft -> Bonsai_web_icon_codicons.comment_draft
    | Comment_unresolved -> Bonsai_web_icon_codicons.comment_unresolved
    | Comment_unresolved_compact -> Bonsai_web_icon_codicons.comment_unresolved_compact
    | Compass -> Bonsai_web_icon_codicons.compass
    | Compass_active -> Bonsai_web_icon_codicons.compass_active
    | Compass_dot -> Bonsai_web_icon_codicons.compass_dot
    | Copilot -> Bonsai_web_icon_codicons.copilot
    | Copilot_blocked -> Bonsai_web_icon_codicons.copilot_blocked
    | Copilot_compact -> Bonsai_web_icon_codicons.copilot_compact
    | Copilot_error -> Bonsai_web_icon_codicons.copilot_error
    | Copilot_in_progress -> Bonsai_web_icon_codicons.copilot_in_progress
    | Copilot_large -> Bonsai_web_icon_codicons.copilot_large
    | Copilot_not_connected -> Bonsai_web_icon_codicons.copilot_not_connected
    | Copilot_snooze -> Bonsai_web_icon_codicons.copilot_snooze
    | Copilot_success -> Bonsai_web_icon_codicons.copilot_success
    | Copilot_unavailable -> Bonsai_web_icon_codicons.copilot_unavailable
    | Copilot_warning -> Bonsai_web_icon_codicons.copilot_warning
    | Copilot_warning_large -> Bonsai_web_icon_codicons.copilot_warning_large
    | Copy -> Bonsai_web_icon_codicons.copy
    | Coverage -> Bonsai_web_icon_codicons.coverage
    | Credit_card -> Bonsai_web_icon_codicons.credit_card
    | Cursor -> Bonsai_web_icon_codicons.cursor
    | Dash -> Bonsai_web_icon_codicons.dash
    | Dashboard -> Bonsai_web_icon_codicons.dashboard
    | Database -> Bonsai_web_icon_codicons.database
    | Debug -> Bonsai_web_icon_codicons.debug
    | Debug_all -> Bonsai_web_icon_codicons.debug_all
    | Debug_alt -> Bonsai_web_icon_codicons.debug_alt
    | Debug_alt_small -> Bonsai_web_icon_codicons.debug_alt_small
    | Debug_breakpoint_conditional ->
      Bonsai_web_icon_codicons.debug_breakpoint_conditional
    | Debug_breakpoint_conditional_unverified ->
      Bonsai_web_icon_codicons.debug_breakpoint_conditional_unverified
    | Debug_breakpoint_data -> Bonsai_web_icon_codicons.debug_breakpoint_data
    | Debug_breakpoint_data_unverified ->
      Bonsai_web_icon_codicons.debug_breakpoint_data_unverified
    | Debug_breakpoint_function -> Bonsai_web_icon_codicons.debug_breakpoint_function
    | Debug_breakpoint_function_unverified ->
      Bonsai_web_icon_codicons.debug_breakpoint_function_unverified
    | Debug_breakpoint_log -> Bonsai_web_icon_codicons.debug_breakpoint_log
    | Debug_breakpoint_log_unverified ->
      Bonsai_web_icon_codicons.debug_breakpoint_log_unverified
    | Debug_breakpoint_unsupported ->
      Bonsai_web_icon_codicons.debug_breakpoint_unsupported
    | Debug_connected -> Bonsai_web_icon_codicons.debug_connected
    | Debug_connected_compact -> Bonsai_web_icon_codicons.debug_connected_compact
    | Debug_console -> Bonsai_web_icon_codicons.debug_console
    | Debug_continue -> Bonsai_web_icon_codicons.debug_continue
    | Debug_continue_small -> Bonsai_web_icon_codicons.debug_continue_small
    | Debug_coverage -> Bonsai_web_icon_codicons.debug_coverage
    | Debug_disconnect -> Bonsai_web_icon_codicons.debug_disconnect
    | Debug_disconnect_compact -> Bonsai_web_icon_codicons.debug_disconnect_compact
    | Debug_line_by_line -> Bonsai_web_icon_codicons.debug_line_by_line
    | Debug_pause -> Bonsai_web_icon_codicons.debug_pause
    | Debug_rerun -> Bonsai_web_icon_codicons.debug_rerun
    | Debug_restart -> Bonsai_web_icon_codicons.debug_restart
    | Debug_restart_frame -> Bonsai_web_icon_codicons.debug_restart_frame
    | Debug_reverse_continue -> Bonsai_web_icon_codicons.debug_reverse_continue
    | Debug_stackframe -> Bonsai_web_icon_codicons.debug_stackframe
    | Debug_stackframe_active -> Bonsai_web_icon_codicons.debug_stackframe_active
    | Debug_start -> Bonsai_web_icon_codicons.debug_start
    | Debug_step_back -> Bonsai_web_icon_codicons.debug_step_back
    | Debug_step_into -> Bonsai_web_icon_codicons.debug_step_into
    | Debug_step_out -> Bonsai_web_icon_codicons.debug_step_out
    | Debug_step_over -> Bonsai_web_icon_codicons.debug_step_over
    | Debug_stop -> Bonsai_web_icon_codicons.debug_stop
    | Desktop_download -> Bonsai_web_icon_codicons.desktop_download
    | Developer_tools -> Bonsai_web_icon_codicons.developer_tools
    | Device_camera -> Bonsai_web_icon_codicons.device_camera
    | Device_camera_video -> Bonsai_web_icon_codicons.device_camera_video
    | Device_mobile -> Bonsai_web_icon_codicons.device_mobile
    | Diff -> Bonsai_web_icon_codicons.diff
    | Diff_added -> Bonsai_web_icon_codicons.diff_added
    | Diff_ignored -> Bonsai_web_icon_codicons.diff_ignored
    | Diff_modified -> Bonsai_web_icon_codicons.diff_modified
    | Diff_multiple -> Bonsai_web_icon_codicons.diff_multiple
    | Diff_removed -> Bonsai_web_icon_codicons.diff_removed
    | Diff_renamed -> Bonsai_web_icon_codicons.diff_renamed
    | Diff_single -> Bonsai_web_icon_codicons.diff_single
    | Discard -> Bonsai_web_icon_codicons.discard
    | Download -> Bonsai_web_icon_codicons.download
    | Edit -> Bonsai_web_icon_codicons.edit
    | Edit_code -> Bonsai_web_icon_codicons.edit_code
    | Edit_compact -> Bonsai_web_icon_codicons.edit_compact
    | Edit_session -> Bonsai_web_icon_codicons.edit_session
    | Edit_sparkle -> Bonsai_web_icon_codicons.edit_sparkle
    | Editor_layout -> Bonsai_web_icon_codicons.editor_layout
    | Ellipsis -> Bonsai_web_icon_codicons.ellipsis
    | Empty_window -> Bonsai_web_icon_codicons.empty_window
    | Eraser -> Bonsai_web_icon_codicons.eraser
    | Error -> Bonsai_web_icon_codicons.error
    | Error_compact -> Bonsai_web_icon_codicons.error_compact
    | Error_small -> Bonsai_web_icon_codicons.error_small
    | Exclude -> Bonsai_web_icon_codicons.exclude
    | Expand_all -> Bonsai_web_icon_codicons.expand_all
    | Export -> Bonsai_web_icon_codicons.export
    | Extensions -> Bonsai_web_icon_codicons.extensions
    | Extensions_large -> Bonsai_web_icon_codicons.extensions_large
    | Eye -> Bonsai_web_icon_codicons.eye
    | Eye_closed -> Bonsai_web_icon_codicons.eye_closed
    | Feedback -> Bonsai_web_icon_codicons.feedback
    | File -> Bonsai_web_icon_codicons.file
    | File_binary -> Bonsai_web_icon_codicons.file_binary
    | File_code -> Bonsai_web_icon_codicons.file_code
    | File_media -> Bonsai_web_icon_codicons.file_media
    | File_media_compact -> Bonsai_web_icon_codicons.file_media_compact
    | File_pdf -> Bonsai_web_icon_codicons.file_pdf
    | File_submodule -> Bonsai_web_icon_codicons.file_submodule
    | File_symlink_directory -> Bonsai_web_icon_codicons.file_symlink_directory
    | File_symlink_file -> Bonsai_web_icon_codicons.file_symlink_file
    | File_text -> Bonsai_web_icon_codicons.file_text
    | File_zip -> Bonsai_web_icon_codicons.file_zip
    | Files -> Bonsai_web_icon_codicons.files
    | Filter -> Bonsai_web_icon_codicons.filter
    | Filter_filled -> Bonsai_web_icon_codicons.filter_filled
    | Fish1_happy -> Bonsai_web_icon_codicons.fish1_happy
    | Fish1_neutral -> Bonsai_web_icon_codicons.fish1_neutral
    | Fish1_sad -> Bonsai_web_icon_codicons.fish1_sad
    | Fish1_very_sad -> Bonsai_web_icon_codicons.fish1_very_sad
    | Fish2_happy -> Bonsai_web_icon_codicons.fish2_happy
    | Fish2_neutral -> Bonsai_web_icon_codicons.fish2_neutral
    | Fish2_sad -> Bonsai_web_icon_codicons.fish2_sad
    | Fish2_very_sad -> Bonsai_web_icon_codicons.fish2_very_sad
    | Fish3_happy -> Bonsai_web_icon_codicons.fish3_happy
    | Fish3_neutral -> Bonsai_web_icon_codicons.fish3_neutral
    | Fish3_sad -> Bonsai_web_icon_codicons.fish3_sad
    | Fish3_very_sad -> Bonsai_web_icon_codicons.fish3_very_sad
    | Fish4_happy -> Bonsai_web_icon_codicons.fish4_happy
    | Fish4_neutral -> Bonsai_web_icon_codicons.fish4_neutral
    | Fish4_sad -> Bonsai_web_icon_codicons.fish4_sad
    | Fish4_very_sad -> Bonsai_web_icon_codicons.fish4_very_sad
    | Flag -> Bonsai_web_icon_codicons.flag
    | Flame -> Bonsai_web_icon_codicons.flame
    | Fold -> Bonsai_web_icon_codicons.fold
    | Fold_down -> Bonsai_web_icon_codicons.fold_down
    | Fold_up -> Bonsai_web_icon_codicons.fold_up
    | Folder -> Bonsai_web_icon_codicons.folder
    | Folder_active -> Bonsai_web_icon_codicons.folder_active
    | Folder_compact -> Bonsai_web_icon_codicons.folder_compact
    | Folder_library -> Bonsai_web_icon_codicons.folder_library
    | Folder_opened -> Bonsai_web_icon_codicons.folder_opened
    | Folder_opened_compact -> Bonsai_web_icon_codicons.folder_opened_compact
    | Forward -> Bonsai_web_icon_codicons.forward
    | Game -> Bonsai_web_icon_codicons.game
    | Gear -> Bonsai_web_icon_codicons.gear
    | Gear_compact -> Bonsai_web_icon_codicons.gear_compact
    | Gift -> Bonsai_web_icon_codicons.gift
    | Gist -> Bonsai_web_icon_codicons.gist
    | Gist_secret -> Bonsai_web_icon_codicons.gist_secret
    | Git_branch -> Bonsai_web_icon_codicons.git_branch
    | Git_branch_changes -> Bonsai_web_icon_codicons.git_branch_changes
    | Git_branch_compact -> Bonsai_web_icon_codicons.git_branch_compact
    | Git_branch_conflicts -> Bonsai_web_icon_codicons.git_branch_conflicts
    | Git_branch_staged_changes -> Bonsai_web_icon_codicons.git_branch_staged_changes
    | Git_commit -> Bonsai_web_icon_codicons.git_commit
    | Git_compare -> Bonsai_web_icon_codicons.git_compare
    | Git_fetch -> Bonsai_web_icon_codicons.git_fetch
    | Git_merge -> Bonsai_web_icon_codicons.git_merge
    | Git_pull_request -> Bonsai_web_icon_codicons.git_pull_request
    | Git_pull_request_closed -> Bonsai_web_icon_codicons.git_pull_request_closed
    | Git_pull_request_comment -> Bonsai_web_icon_codicons.git_pull_request_comment
    | Git_pull_request_create -> Bonsai_web_icon_codicons.git_pull_request_create
    | Git_pull_request_done -> Bonsai_web_icon_codicons.git_pull_request_done
    | Git_pull_request_draft -> Bonsai_web_icon_codicons.git_pull_request_draft
    | Git_pull_request_error -> Bonsai_web_icon_codicons.git_pull_request_error
    | Git_pull_request_go_to_changes ->
      Bonsai_web_icon_codicons.git_pull_request_go_to_changes
    | Git_pull_request_new_changes ->
      Bonsai_web_icon_codicons.git_pull_request_new_changes
    | Git_stash -> Bonsai_web_icon_codicons.git_stash
    | Git_stash_apply -> Bonsai_web_icon_codicons.git_stash_apply
    | Git_stash_pop -> Bonsai_web_icon_codicons.git_stash_pop
    | Github -> Bonsai_web_icon_codicons.github
    | Github_action -> Bonsai_web_icon_codicons.github_action
    | Github_alt -> Bonsai_web_icon_codicons.github_alt
    | Github_inverted -> Bonsai_web_icon_codicons.github_inverted
    | Github_project -> Bonsai_web_icon_codicons.github_project
    | Globe -> Bonsai_web_icon_codicons.globe
    | Go_to_editing_session -> Bonsai_web_icon_codicons.go_to_editing_session
    | Go_to_file -> Bonsai_web_icon_codicons.go_to_file
    | Go_to_search -> Bonsai_web_icon_codicons.go_to_search
    | Google_gemini -> Bonsai_web_icon_codicons.google_gemini
    | Grabber -> Bonsai_web_icon_codicons.grabber
    | Graph -> Bonsai_web_icon_codicons.graph
    | Graph_left -> Bonsai_web_icon_codicons.graph_left
    | Graph_line -> Bonsai_web_icon_codicons.graph_line
    | Graph_scatter -> Bonsai_web_icon_codicons.graph_scatter
    | Gripper -> Bonsai_web_icon_codicons.gripper
    | Group_by_ref_type -> Bonsai_web_icon_codicons.group_by_ref_type
    | Heart -> Bonsai_web_icon_codicons.heart
    | Heart_filled -> Bonsai_web_icon_codicons.heart_filled
    | History -> Bonsai_web_icon_codicons.history
    | Home -> Bonsai_web_icon_codicons.home
    | Horizontal_rule -> Bonsai_web_icon_codicons.horizontal_rule
    | Hubot -> Bonsai_web_icon_codicons.hubot
    | Important -> Bonsai_web_icon_codicons.important
    | Important_compact -> Bonsai_web_icon_codicons.important_compact
    | Inbox -> Bonsai_web_icon_codicons.inbox
    | Indent -> Bonsai_web_icon_codicons.indent
    | Index_zero -> Bonsai_web_icon_codicons.index_zero
    | Info -> Bonsai_web_icon_codicons.info
    | Insert -> Bonsai_web_icon_codicons.insert
    | Inspect -> Bonsai_web_icon_codicons.inspect
    | Issue_draft -> Bonsai_web_icon_codicons.issue_draft
    | Issue_reopened -> Bonsai_web_icon_codicons.issue_reopened
    | Issues -> Bonsai_web_icon_codicons.issues
    | Italic -> Bonsai_web_icon_codicons.italic
    | Jersey -> Bonsai_web_icon_codicons.jersey
    | Json -> Bonsai_web_icon_codicons.json
    | Kebab_vertical -> Bonsai_web_icon_codicons.kebab_vertical
    | Key -> Bonsai_web_icon_codicons.key
    | Keyboard_tab -> Bonsai_web_icon_codicons.keyboard_tab
    | Keyboard_tab_above -> Bonsai_web_icon_codicons.keyboard_tab_above
    | Keyboard_tab_below -> Bonsai_web_icon_codicons.keyboard_tab_below
    | Kimi -> Bonsai_web_icon_codicons.kimi
    | Law -> Bonsai_web_icon_codicons.law
    | Layers -> Bonsai_web_icon_codicons.layers
    | Layers_active -> Bonsai_web_icon_codicons.layers_active
    | Layers_dot -> Bonsai_web_icon_codicons.layers_dot
    | Layout -> Bonsai_web_icon_codicons.layout
    | Layout_activitybar_left -> Bonsai_web_icon_codicons.layout_activitybar_left
    | Layout_activitybar_right -> Bonsai_web_icon_codicons.layout_activitybar_right
    | Layout_centered -> Bonsai_web_icon_codicons.layout_centered
    | Layout_menubar -> Bonsai_web_icon_codicons.layout_menubar
    | Layout_panel -> Bonsai_web_icon_codicons.layout_panel
    | Layout_panel_center -> Bonsai_web_icon_codicons.layout_panel_center
    | Layout_panel_dock -> Bonsai_web_icon_codicons.layout_panel_dock
    | Layout_panel_justify -> Bonsai_web_icon_codicons.layout_panel_justify
    | Layout_panel_left -> Bonsai_web_icon_codicons.layout_panel_left
    | Layout_panel_off -> Bonsai_web_icon_codicons.layout_panel_off
    | Layout_panel_right -> Bonsai_web_icon_codicons.layout_panel_right
    | Layout_sidebar_left -> Bonsai_web_icon_codicons.layout_sidebar_left
    | Layout_sidebar_left_dock -> Bonsai_web_icon_codicons.layout_sidebar_left_dock
    | Layout_sidebar_left_off -> Bonsai_web_icon_codicons.layout_sidebar_left_off
    | Layout_sidebar_right -> Bonsai_web_icon_codicons.layout_sidebar_right
    | Layout_sidebar_right_dock -> Bonsai_web_icon_codicons.layout_sidebar_right_dock
    | Layout_sidebar_right_off -> Bonsai_web_icon_codicons.layout_sidebar_right_off
    | Layout_statusbar -> Bonsai_web_icon_codicons.layout_statusbar
    | Library -> Bonsai_web_icon_codicons.library
    | Library_compact -> Bonsai_web_icon_codicons.library_compact
    | Lightbulb -> Bonsai_web_icon_codicons.lightbulb
    | Lightbulb_autofix -> Bonsai_web_icon_codicons.lightbulb_autofix
    | Lightbulb_compact -> Bonsai_web_icon_codicons.lightbulb_compact
    | Lightbulb_empty -> Bonsai_web_icon_codicons.lightbulb_empty
    | Lightbulb_sparkle -> Bonsai_web_icon_codicons.lightbulb_sparkle
    | Link -> Bonsai_web_icon_codicons.link
    | Link_external -> Bonsai_web_icon_codicons.link_external
    | List_filter -> Bonsai_web_icon_codicons.list_filter
    | List_flat -> Bonsai_web_icon_codicons.list_flat
    | List_ordered -> Bonsai_web_icon_codicons.list_ordered
    | List_selection -> Bonsai_web_icon_codicons.list_selection
    | List_tree -> Bonsai_web_icon_codicons.list_tree
    | List_unordered -> Bonsai_web_icon_codicons.list_unordered
    | Live_share -> Bonsai_web_icon_codicons.live_share
    | Loading -> Bonsai_web_icon_codicons.loading
    | Loading_compact -> Bonsai_web_icon_codicons.loading_compact
    | Location -> Bonsai_web_icon_codicons.location
    | Lock -> Bonsai_web_icon_codicons.lock
    | Lock_small -> Bonsai_web_icon_codicons.lock_small
    | Magnet -> Bonsai_web_icon_codicons.magnet
    | Mail -> Bonsai_web_icon_codicons.mail
    | Mail_read -> Bonsai_web_icon_codicons.mail_read
    | Map -> Bonsai_web_icon_codicons.map
    | Map_filled -> Bonsai_web_icon_codicons.map_filled
    | Map_vertical -> Bonsai_web_icon_codicons.map_vertical
    | Map_vertical_filled -> Bonsai_web_icon_codicons.map_vertical_filled
    | Markdown -> Bonsai_web_icon_codicons.markdown
    | Mcp -> Bonsai_web_icon_codicons.mcp
    | Megaphone -> Bonsai_web_icon_codicons.megaphone
    | Mention -> Bonsai_web_icon_codicons.mention
    | Menu -> Bonsai_web_icon_codicons.menu
    | Merge -> Bonsai_web_icon_codicons.merge
    | Merge_into -> Bonsai_web_icon_codicons.merge_into
    | Mic -> Bonsai_web_icon_codicons.mic
    | Mic_compact -> Bonsai_web_icon_codicons.mic_compact
    | Mic_download -> Bonsai_web_icon_codicons.mic_download
    | Mic_download_compact -> Bonsai_web_icon_codicons.mic_download_compact
    | Mic_filled -> Bonsai_web_icon_codicons.mic_filled
    | Microsoft -> Bonsai_web_icon_codicons.microsoft
    | Milestone -> Bonsai_web_icon_codicons.milestone
    | Mirror -> Bonsai_web_icon_codicons.mirror
    | Mortar_board -> Bonsai_web_icon_codicons.mortar_board
    | Move -> Bonsai_web_icon_codicons.move
    | Multiple_windows -> Bonsai_web_icon_codicons.multiple_windows
    | Music -> Bonsai_web_icon_codicons.music
    | Mute -> Bonsai_web_icon_codicons.mute
    | New_collection -> Bonsai_web_icon_codicons.new_collection
    | New_file -> Bonsai_web_icon_codicons.new_file
    | New_folder -> Bonsai_web_icon_codicons.new_folder
    | New_session -> Bonsai_web_icon_codicons.new_session
    | Newline -> Bonsai_web_icon_codicons.newline
    | No_newline -> Bonsai_web_icon_codicons.no_newline
    | Note -> Bonsai_web_icon_codicons.note
    | Notebook -> Bonsai_web_icon_codicons.notebook
    | Notebook_template -> Bonsai_web_icon_codicons.notebook_template
    | Octoface -> Bonsai_web_icon_codicons.octoface
    | Open_in_product -> Bonsai_web_icon_codicons.open_in_product
    | Open_in_window -> Bonsai_web_icon_codicons.open_in_window
    | Open_preview -> Bonsai_web_icon_codicons.open_preview
    | Openai -> Bonsai_web_icon_codicons.openai
    | Organization -> Bonsai_web_icon_codicons.organization
    | Output -> Bonsai_web_icon_codicons.output
    | Package -> Bonsai_web_icon_codicons.package
    | Paintcan -> Bonsai_web_icon_codicons.paintcan
    | Pass -> Bonsai_web_icon_codicons.pass
    | Pass_compact -> Bonsai_web_icon_codicons.pass_compact
    | Pass_filled -> Bonsai_web_icon_codicons.pass_filled
    | Pass_filled_compact -> Bonsai_web_icon_codicons.pass_filled_compact
    | Percentage -> Bonsai_web_icon_codicons.percentage
    | Person -> Bonsai_web_icon_codicons.person
    | Person_add -> Bonsai_web_icon_codicons.person_add
    | Person_voice -> Bonsai_web_icon_codicons.person_voice
    | Person_voice_compact -> Bonsai_web_icon_codicons.person_voice_compact
    | Person_voice_filled -> Bonsai_web_icon_codicons.person_voice_filled
    | Person_voice_filled_compact -> Bonsai_web_icon_codicons.person_voice_filled_compact
    | Piano -> Bonsai_web_icon_codicons.piano
    | Pie_chart -> Bonsai_web_icon_codicons.pie_chart
    | Pin -> Bonsai_web_icon_codicons.pin
    | Pinned -> Bonsai_web_icon_codicons.pinned
    | Pinned_dirty -> Bonsai_web_icon_codicons.pinned_dirty
    | Play -> Bonsai_web_icon_codicons.play
    | Play_circle -> Bonsai_web_icon_codicons.play_circle
    | Plug -> Bonsai_web_icon_codicons.plug
    | Preserve_case -> Bonsai_web_icon_codicons.preserve_case
    | Preview -> Bonsai_web_icon_codicons.preview
    | Primitive_square -> Bonsai_web_icon_codicons.primitive_square
    | Project -> Bonsai_web_icon_codicons.project
    | Project_compact -> Bonsai_web_icon_codicons.project_compact
    | Pulse -> Bonsai_web_icon_codicons.pulse
    | Python -> Bonsai_web_icon_codicons.python
    | Question -> Bonsai_web_icon_codicons.question
    | Quote -> Bonsai_web_icon_codicons.quote
    | Quotes -> Bonsai_web_icon_codicons.quotes
    | Radio_tower -> Bonsai_web_icon_codicons.radio_tower
    | Reactions -> Bonsai_web_icon_codicons.reactions
    | Record -> Bonsai_web_icon_codicons.record
    | Record_keys -> Bonsai_web_icon_codicons.record_keys
    | Record_keys_compact -> Bonsai_web_icon_codicons.record_keys_compact
    | Record_small -> Bonsai_web_icon_codicons.record_small
    | Redo -> Bonsai_web_icon_codicons.redo
    | References -> Bonsai_web_icon_codicons.references
    | Refresh -> Bonsai_web_icon_codicons.refresh
    | Refresh_compact -> Bonsai_web_icon_codicons.refresh_compact
    | Regex -> Bonsai_web_icon_codicons.regex
    | Remote -> Bonsai_web_icon_codicons.remote
    | Remote_compact -> Bonsai_web_icon_codicons.remote_compact
    | Remote_explorer -> Bonsai_web_icon_codicons.remote_explorer
    | Remove -> Bonsai_web_icon_codicons.remove
    | Remove_small -> Bonsai_web_icon_codicons.remove_small
    | Rename -> Bonsai_web_icon_codicons.rename
    | Replace -> Bonsai_web_icon_codicons.replace
    | Replace_all -> Bonsai_web_icon_codicons.replace_all
    | Reply -> Bonsai_web_icon_codicons.reply
    | Repo -> Bonsai_web_icon_codicons.repo
    | Repo_clone -> Bonsai_web_icon_codicons.repo_clone
    | Repo_compact -> Bonsai_web_icon_codicons.repo_compact
    | Repo_fetch -> Bonsai_web_icon_codicons.repo_fetch
    | Repo_force_push -> Bonsai_web_icon_codicons.repo_force_push
    | Repo_forked -> Bonsai_web_icon_codicons.repo_forked
    | Repo_forked_compact -> Bonsai_web_icon_codicons.repo_forked_compact
    | Repo_pinned -> Bonsai_web_icon_codicons.repo_pinned
    | Repo_pull -> Bonsai_web_icon_codicons.repo_pull
    | Repo_push -> Bonsai_web_icon_codicons.repo_push
    | Repo_selected -> Bonsai_web_icon_codicons.repo_selected
    | Report -> Bonsai_web_icon_codicons.report
    | Request_changes -> Bonsai_web_icon_codicons.request_changes
    | Right_panel_hide -> Bonsai_web_icon_codicons.right_panel_hide
    | Right_panel_show -> Bonsai_web_icon_codicons.right_panel_show
    | Robot -> Bonsai_web_icon_codicons.robot
    | Rocket -> Bonsai_web_icon_codicons.rocket
    | Rocket_compact -> Bonsai_web_icon_codicons.rocket_compact
    | Root_folder -> Bonsai_web_icon_codicons.root_folder
    | Root_folder_opened -> Bonsai_web_icon_codicons.root_folder_opened
    | Rss -> Bonsai_web_icon_codicons.rss
    | Ruby -> Bonsai_web_icon_codicons.ruby
    | Run_above -> Bonsai_web_icon_codicons.run_above
    | Run_all -> Bonsai_web_icon_codicons.run_all
    | Run_all_coverage -> Bonsai_web_icon_codicons.run_all_coverage
    | Run_below -> Bonsai_web_icon_codicons.run_below
    | Run_compact -> Bonsai_web_icon_codicons.run_compact
    | Run_coverage -> Bonsai_web_icon_codicons.run_coverage
    | Run_errors -> Bonsai_web_icon_codicons.run_errors
    | Run_with_deps -> Bonsai_web_icon_codicons.run_with_deps
    | Save -> Bonsai_web_icon_codicons.save
    | Save_all -> Bonsai_web_icon_codicons.save_all
    | Save_as -> Bonsai_web_icon_codicons.save_as
    | Screen_cut -> Bonsai_web_icon_codicons.screen_cut
    | Screen_full -> Bonsai_web_icon_codicons.screen_full
    | Screen_normal -> Bonsai_web_icon_codicons.screen_normal
    | Search -> Bonsai_web_icon_codicons.search
    | Search_compact -> Bonsai_web_icon_codicons.search_compact
    | Search_fuzzy -> Bonsai_web_icon_codicons.search_fuzzy
    | Search_large -> Bonsai_web_icon_codicons.search_large
    | Search_sparkle -> Bonsai_web_icon_codicons.search_sparkle
    | Search_stop -> Bonsai_web_icon_codicons.search_stop
    | Send -> Bonsai_web_icon_codicons.send
    | Send_to_remote_agent -> Bonsai_web_icon_codicons.send_to_remote_agent
    | Server -> Bonsai_web_icon_codicons.server
    | Server_environment -> Bonsai_web_icon_codicons.server_environment
    | Server_process -> Bonsai_web_icon_codicons.server_process
    | Session_in_progress -> Bonsai_web_icon_codicons.session_in_progress
    | Session_in_progress_compact -> Bonsai_web_icon_codicons.session_in_progress_compact
    | Settings -> Bonsai_web_icon_codicons.settings
    | Settings_compact -> Bonsai_web_icon_codicons.settings_compact
    | Settings_gear -> Bonsai_web_icon_codicons.settings_gear
    | Share -> Bonsai_web_icon_codicons.share
    | Share_window -> Bonsai_web_icon_codicons.share_window
    | Shield -> Bonsai_web_icon_codicons.shield
    | Shield_compact -> Bonsai_web_icon_codicons.shield_compact
    | Sign_in -> Bonsai_web_icon_codicons.sign_in
    | Sign_out -> Bonsai_web_icon_codicons.sign_out
    | Skip -> Bonsai_web_icon_codicons.skip
    | Smiley -> Bonsai_web_icon_codicons.smiley
    | Snake -> Bonsai_web_icon_codicons.snake
    | Sort_precedence -> Bonsai_web_icon_codicons.sort_precedence
    | Source_control -> Bonsai_web_icon_codicons.source_control
    | Sparkle -> Bonsai_web_icon_codicons.sparkle
    | Sparkle_compact -> Bonsai_web_icon_codicons.sparkle_compact
    | Sparkle_filled -> Bonsai_web_icon_codicons.sparkle_filled
    | Split_horizontal -> Bonsai_web_icon_codicons.split_horizontal
    | Split_vertical -> Bonsai_web_icon_codicons.split_vertical
    | Squirrel -> Bonsai_web_icon_codicons.squirrel
    | Star_empty -> Bonsai_web_icon_codicons.star_empty
    | Star_full -> Bonsai_web_icon_codicons.star_full
    | Star_half -> Bonsai_web_icon_codicons.star_half
    | Stop_circle -> Bonsai_web_icon_codicons.stop_circle
    | Strikethrough -> Bonsai_web_icon_codicons.strikethrough
    | Surround_with -> Bonsai_web_icon_codicons.surround_with
    | Symbol_array -> Bonsai_web_icon_codicons.symbol_array
    | Symbol_boolean -> Bonsai_web_icon_codicons.symbol_boolean
    | Symbol_class -> Bonsai_web_icon_codicons.symbol_class
    | Symbol_color -> Bonsai_web_icon_codicons.symbol_color
    | Symbol_color_compact -> Bonsai_web_icon_codicons.symbol_color_compact
    | Symbol_constant -> Bonsai_web_icon_codicons.symbol_constant
    | Symbol_enum -> Bonsai_web_icon_codicons.symbol_enum
    | Symbol_enum_member -> Bonsai_web_icon_codicons.symbol_enum_member
    | Symbol_event -> Bonsai_web_icon_codicons.symbol_event
    | Symbol_field -> Bonsai_web_icon_codicons.symbol_field
    | Symbol_file -> Bonsai_web_icon_codicons.symbol_file
    | Symbol_interface -> Bonsai_web_icon_codicons.symbol_interface
    | Symbol_key -> Bonsai_web_icon_codicons.symbol_key
    | Symbol_keyword -> Bonsai_web_icon_codicons.symbol_keyword
    | Symbol_method -> Bonsai_web_icon_codicons.symbol_method
    | Symbol_method_arrow -> Bonsai_web_icon_codicons.symbol_method_arrow
    | Symbol_misc -> Bonsai_web_icon_codicons.symbol_misc
    | Symbol_namespace -> Bonsai_web_icon_codicons.symbol_namespace
    | Symbol_numeric -> Bonsai_web_icon_codicons.symbol_numeric
    | Symbol_operator -> Bonsai_web_icon_codicons.symbol_operator
    | Symbol_parameter -> Bonsai_web_icon_codicons.symbol_parameter
    | Symbol_property -> Bonsai_web_icon_codicons.symbol_property
    | Symbol_ruler -> Bonsai_web_icon_codicons.symbol_ruler
    | Symbol_snippet -> Bonsai_web_icon_codicons.symbol_snippet
    | Symbol_string -> Bonsai_web_icon_codicons.symbol_string
    | Symbol_structure -> Bonsai_web_icon_codicons.symbol_structure
    | Symbol_variable -> Bonsai_web_icon_codicons.symbol_variable
    | Sync -> Bonsai_web_icon_codicons.sync
    | Sync_compact -> Bonsai_web_icon_codicons.sync_compact
    | Sync_ignored -> Bonsai_web_icon_codicons.sync_ignored
    | Table -> Bonsai_web_icon_codicons.table
    | Tag -> Bonsai_web_icon_codicons.tag
    | Target -> Bonsai_web_icon_codicons.target
    | Tasklist -> Bonsai_web_icon_codicons.tasklist
    | Telescope -> Bonsai_web_icon_codicons.telescope
    | Terminal -> Bonsai_web_icon_codicons.terminal
    | Terminal_bash -> Bonsai_web_icon_codicons.terminal_bash
    | Terminal_cmd -> Bonsai_web_icon_codicons.terminal_cmd
    | Terminal_compact -> Bonsai_web_icon_codicons.terminal_compact
    | Terminal_debian -> Bonsai_web_icon_codicons.terminal_debian
    | Terminal_git_bash -> Bonsai_web_icon_codicons.terminal_git_bash
    | Terminal_linux -> Bonsai_web_icon_codicons.terminal_linux
    | Terminal_powershell -> Bonsai_web_icon_codicons.terminal_powershell
    | Terminal_secure -> Bonsai_web_icon_codicons.terminal_secure
    | Terminal_tmux -> Bonsai_web_icon_codicons.terminal_tmux
    | Terminal_ubuntu -> Bonsai_web_icon_codicons.terminal_ubuntu
    | Text_size -> Bonsai_web_icon_codicons.text_size
    | Thinking -> Bonsai_web_icon_codicons.thinking
    | Three_bars -> Bonsai_web_icon_codicons.three_bars
    | Thumbsdown -> Bonsai_web_icon_codicons.thumbsdown
    | Thumbsdown_filled -> Bonsai_web_icon_codicons.thumbsdown_filled
    | Thumbsup -> Bonsai_web_icon_codicons.thumbsup
    | Thumbsup_filled -> Bonsai_web_icon_codicons.thumbsup_filled
    | Tools -> Bonsai_web_icon_codicons.tools
    | Trash -> Bonsai_web_icon_codicons.trash
    | Triangle_down -> Bonsai_web_icon_codicons.triangle_down
    | Triangle_left -> Bonsai_web_icon_codicons.triangle_left
    | Triangle_right -> Bonsai_web_icon_codicons.triangle_right
    | Triangle_up -> Bonsai_web_icon_codicons.triangle_up
    | Twitter -> Bonsai_web_icon_codicons.twitter
    | Type_hierarchy -> Bonsai_web_icon_codicons.type_hierarchy
    | Type_hierarchy_sub -> Bonsai_web_icon_codicons.type_hierarchy_sub
    | Type_hierarchy_super -> Bonsai_web_icon_codicons.type_hierarchy_super
    | Unarchive -> Bonsai_web_icon_codicons.unarchive
    | Unfold -> Bonsai_web_icon_codicons.unfold
    | Ungroup_by_ref_type -> Bonsai_web_icon_codicons.ungroup_by_ref_type
    | Unlock -> Bonsai_web_icon_codicons.unlock
    | Unmute -> Bonsai_web_icon_codicons.unmute
    | Unpin -> Bonsai_web_icon_codicons.unpin
    | Unverified -> Bonsai_web_icon_codicons.unverified
    | Variable_group -> Bonsai_web_icon_codicons.variable_group
    | Verified -> Bonsai_web_icon_codicons.verified
    | Verified_filled -> Bonsai_web_icon_codicons.verified_filled
    | Versions -> Bonsai_web_icon_codicons.versions
    | Vm -> Bonsai_web_icon_codicons.vm
    | Vm_active -> Bonsai_web_icon_codicons.vm_active
    | Vm_compact -> Bonsai_web_icon_codicons.vm_compact
    | Vm_connect -> Bonsai_web_icon_codicons.vm_connect
    | Vm_outline -> Bonsai_web_icon_codicons.vm_outline
    | Vm_pending -> Bonsai_web_icon_codicons.vm_pending
    | Vm_running -> Bonsai_web_icon_codicons.vm_running
    | Vm_small -> Bonsai_web_icon_codicons.vm_small
    | Voice_mode -> Bonsai_web_icon_codicons.voice_mode
    | Voice_mode_compact -> Bonsai_web_icon_codicons.voice_mode_compact
    | Voice_mode_download -> Bonsai_web_icon_codicons.voice_mode_download
    | Voice_mode_download_compact -> Bonsai_web_icon_codicons.voice_mode_download_compact
    | Vr -> Bonsai_web_icon_codicons.vr
    | Vscode -> Bonsai_web_icon_codicons.vscode
    | Vscode_insiders -> Bonsai_web_icon_codicons.vscode_insiders
    | Vscode_insiders_outline -> Bonsai_web_icon_codicons.vscode_insiders_outline
    | Vscode_outline -> Bonsai_web_icon_codicons.vscode_outline
    | Wand -> Bonsai_web_icon_codicons.wand
    | Warning -> Bonsai_web_icon_codicons.warning
    | Warning_compact -> Bonsai_web_icon_codicons.warning_compact
    | Watch -> Bonsai_web_icon_codicons.watch
    | Whitespace -> Bonsai_web_icon_codicons.whitespace
    | Whole_word -> Bonsai_web_icon_codicons.whole_word
    | Window -> Bonsai_web_icon_codicons.window
    | Window_active -> Bonsai_web_icon_codicons.window_active
    | Window_compact -> Bonsai_web_icon_codicons.window_compact
    | Word_wrap -> Bonsai_web_icon_codicons.word_wrap
    | Workspace_trusted -> Bonsai_web_icon_codicons.workspace_trusted
    | Workspace_unknown -> Bonsai_web_icon_codicons.workspace_unknown
    | Workspace_untrusted -> Bonsai_web_icon_codicons.workspace_untrusted
    | Worktree -> Bonsai_web_icon_codicons.worktree
    | Worktree_compact -> Bonsai_web_icon_codicons.worktree_compact
    | Worktree_small -> Bonsai_web_icon_codicons.worktree_small
    | Xai -> Bonsai_web_icon_codicons.xai
    | Zoom_in -> Bonsai_web_icon_codicons.zoom_in
    | Zoom_out -> Bonsai_web_icon_codicons.zoom_out
  ;;
end
