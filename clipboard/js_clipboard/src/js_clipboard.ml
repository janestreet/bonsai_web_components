(** Wrapper around the browser interface to the system clipboard.

    There are two standard ways of accessing the system clipboard from Javascript. This is
    because browsers are rightfully scared of privacy / cyber security implications of
    arbitrary webpages reading the clipboard without an explicit consent / knowlege of the
    user.

    # 1. Synchronous Clipboard API

    This API solves the consent / knowledge problem by replacing the event handlers that
    are run in response to the user pressing the default keyboard shortcuts for copy /
    paste AND requiring that all modification of the clipboard are done as part of that
    event handler.

    For `copy` and `cut`, one can supply a custom content that will be put into the
    clipboard. For `paste`, one can run a custom action, preventing the default. At the
    time of writing, no permission restrictions apply to the Synchronous API.

    https://www.w3.org/TR/clipboard-apis/#sync-clipboard-api

    # 2. Asynchronous Clipboard API

    This is the more powerful API. But it comes with more burden. Reading the clipboard
    and writing to it at arbitrary point in time, by calling specific Javascript
    functions. This works only on HTTPS websites and while the tab is focused. User must
    explicitly grant the permission for reading clipboard contents.

    https://www.w3.org/TR/clipboard-apis/#async-clipboard-api

    Most of the time you will want to use the Synchronous API, as it is easier to use, and
    more guaranteed to work. *)

module Synchronous = Synchronous
module Asynchronous = Asynchronous
