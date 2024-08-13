open! Core
open! Import

(** A simple way to construct feather icon svgs for use in jsoo.

    A few helpful resources:
    - The external feather icons website: https://feathericons.com/
    - Our bonsai demo equivalent: https://bonsai:8548/
    - The code for the bonsai demo: ../../bonsai/examples/feather_icons
*)

type t =
  | Activity
  | Corner_down_left
  | Link
  | Shopping_bag
  | Airplay
  | Corner_down_right
  | List
  | Shopping_cart
  | Alert_circle
  | Corner_left_down
  | Loader
  | Shuffle
  | Alert_octagon
  | Corner_left_up
  | Lock
  | Sidebar
  | Alert_triangle
  | Corner_right_down
  | Log_in
  | Skip_back
  | Align_center
  | Corner_right_up
  | Log_out
  | Skip_forward
  | Align_justify
  | Corner_up_left
  | Mail
  | Slack
  | Align_left
  | Corner_up_right
  | Map_pin
  | Slash
  | Align_right
  | Cpu
  | Map
  | Sliders
  | Anchor
  | Credit_card
  | Maximize_2
  | Smartphone
  | Aperture
  | Crop
  | Maximize
  | Smile
  | Archive
  | Crosshair
  | Meh
  | Speaker
  | Arrow_down_circle
  | Database
  | Menu
  | Square
  | Arrow_down_left
  | Delete
  | Message_circle
  | Star
  | Arrow_down_right
  | Disc
  | Message_square
  | Stop_circle
  | Arrow_down
  | Divide_circle
  | Mic_off
  | Sunrise
  | Arrow_left_circle
  | Divide_square
  | Mic
  | Sunset
  | Arrow_left
  | Divide
  | Minimize_2
  | Sun
  | Arrow_right_circle
  | Dollar_sign
  | Minimize
  | Tablet
  | Arrow_right
  | Download_cloud
  | Minus_circle
  | Tag
  | Arrow_up_circle
  | Download
  | Minus_square
  | Target
  | Arrow_up_left
  | Dribbble
  | Minus
  | Terminal
  | Arrow_up_right
  | Droplet
  | Monitor
  | Thermometer
  | Arrow_up
  | Edit_2
  | Moon
  | Thumbs_down
  | At_sign
  | Edit_3
  | More_horizontal
  | Thumbs_up
  | Award
  | Edit
  | More_vertical
  | Toggle_left
  | Bar_chart_2
  | External_link
  | Mouse_pointer
  | Toggle_right
  | Bar_chart
  | Eye_off
  | Move
  | Tool
  | Battery_charging
  | Eye
  | Music
  | Trash_2
  | Battery
  | Facebook
  | Navigation_2
  | Trash
  | Bell_off
  | Fast_forward
  | Navigation
  | Trello
  | Bell
  | Feather
  | Octagon
  | Trending_down
  | Bluetooth
  | Figma
  | Package
  | Trending_up
  | Bold
  | File_minus
  | Paperclip
  | Triangle
  | Bookmark
  | File_plus
  | Pause_circle
  | Truck
  | Book_open
  | File
  | Pause
  | Tv
  | Book
  | File_text
  | Pen_tool
  | Twitch
  | Box
  | Film
  | Percent
  | Twitter
  | Briefcase
  | Filter
  | Phone_call
  | Type
  | Calendar
  | Flag
  | Phone_forwarded
  | Umbrella
  | Camera_off
  | Folder_minus
  | Phone_incoming
  | Underline
  | Camera
  | Folder_plus
  | Phone_missed
  | Unlock
  | Cast
  | Folder
  | Phone_off
  | Upload_cloud
  | Check_circle
  | Framer
  | Phone_outgoing
  | Upload
  | Check_square
  | Frown
  | Phone
  | User_check
  | Check
  | Gift
  | Pie_chart
  | User_minus
  | Chevron_down
  | Git_branch
  | Play_circle
  | User_plus
  | Chevron_left
  | Git_commit
  | Play
  | Users
  | Chevron_right
  | Github
  | Plus_circle
  | User
  | Chevrons_down
  | Gitlab
  | Plus_square
  | User_x
  | Chevrons_left
  | Git_merge
  | Plus
  | Video_off
  | Chevrons_right
  | Git_pull_request
  | Pocket
  | Video
  | Chevrons_up
  | Globe
  | Power
  | Voicemail
  | Chevron_up
  | Grid
  | Printer
  | Volume_1
  | Chrome
  | Hard_drive
  | Radio
  | Volume_2
  | Circle
  | Hash
  | Refresh_ccw
  | Volume
  | Clipboard
  | Headphones
  | Refresh_cw
  | Volume_x
  | Clock
  | Heart
  | Repeat
  | Watch
  | Cloud_drizzle
  | Help_circle
  | Rewind
  | Wifi_off
  | Cloud_lightning
  | Hexagon
  | Rotate_ccw
  | Wifi
  | Cloud_off
  | Home
  | Rotate_cw
  | Wind
  | Cloud_rain
  | Image
  | Rss
  | X_circle
  | Cloud_snow
  | Inbox
  | Save
  | X_octagon
  | Cloud
  | Info
  | Scissors
  | X_square
  | Codepen
  | Instagram
  | Search
  | X
  | Codesandbox
  | Italic
  | Send
  | Youtube
  | Code
  | Key
  | Server
  | Zap_off
  | Coffee
  | Layers
  | Settings
  | Zap
  | Columns
  | Layout
  | Share_2
  | Zoom_in
  | Command
  | Life_buoy
  | Share
  | Zoom_out
  | Compass
  | Link_2
  | Shield_off
  | Copy
  | Linkedin
  | Shield
[@@deriving compare, enumerate, equal, sexp, sexp_grammar]

(** Useful for favicons and css backgrounds *)
val svg_string
  :  ?size:[< Css_gen.Length.t ]
  -> ?stroke:[< Css_gen.Color.t ]
  -> ?fill:[< Css_gen.Color.t ]
  -> ?stroke_width:[< Css_gen.Length.t ]
  -> t
  -> string

(* $MDX part-begin=svg *)

val svg
  :  ?size:[< Css_gen.Length.t ]
  -> ?stroke:[< Css_gen.Color.t ]
  -> ?fill:[< Css_gen.Color.t ]
  -> ?stroke_width:[< Css_gen.Length.t ]
  -> ?extra_attrs:Vdom.Attr.t list
  -> t
  -> Vdom.Node.t

(* $MDX part-end *)

val to_string : t -> string
