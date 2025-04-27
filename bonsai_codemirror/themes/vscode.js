/* Editor UI colors */
var darkColors = {
    editorForeground: 'var(--vscode-editor-foreground, #e3e3e3)',
    editorBackground: 'var(--vscode-editor-background, #1F1F1F)',
    editorSelectionBackground: 'var(--vscode-editor-selectionBackground, #0080ff66)',
    editorLineNumberForeground: 'var(--vscode-editorLineNumber-foreground, #6E7681)',
    editorMatchBackground: '#FBA00966',
    editorSelectedMatchBackground: '#FBA00999',
    panelForeground: 'var(--vscode-panel-foreground, #CCCCCC)',
    panelBackground: 'var(--vscode-panel-background, #181818)',
    panelBorder: 'var(--vscode-panel-border, #2B2B2B)',
    focusedOutline: 'var(--skyline-color-accent, #3794FF)',
    tokenOperator: '#C586C0',
    tokenValue: '#4FC1FF',
    tokenLiteral: '#DCDCAA',
    tokenType: '#C5CEA8',
};
var lightColors = {
    editorForeground: 'var(--vscode-editor-foreground, #3B3B3B)',
    editorBackground: 'var(--vscode-editor-background, #FFFFFF)',
    editorSelectionBackground: 'var(--vscode-editor-selectionBackground, #0080ff66)',
    editorLineNumberForeground: 'var(--vscode-editorLineNumber-foreground, #6E7681)',
    editorMatchBackground: '#BB800966',
    editorSelectedMatchBackground: '#BB800999',
    panelForeground: 'var(--vscode-panel-foreground, #3B3B3B)',
    panelBackground: 'var(--vscode-panel-background, #F8F8F8)',
    panelBorder: 'var(--vscode-panel-border, #E5E5E5)',
    focusedOutline: 'var(--skyline-color-accent, #1f7eeb)',
    tokenOperator: '#AF00DB',
    tokenValue: '#0070C1',
    tokenLiteral: '#795E26',
    tokenType: '#098658',
};

var defaultColors = {};
for (const key of Object.getOwnPropertyNames(darkColors)) {
    defaultColors[key] = `light-dark(${lightColors[key]}, ${darkColors[key]})`;
}

/* Token colors */
var darkTokenColors = {
    tokenFunction: '#DCDCAA',
    tokenType: '#4EC9B0',
    tokenKeyword: '#569CD6',
    tokenPunctuation: darkColors.editorForeground,
    tokenParens: '#C586C0',
    tokenVarName: '#9CDCFE',
    tokenConstant: '#4FC1FF',
    tokenPropery: '#CE9178',
    tokenComment: '#6A9955',
    tokenString: '#CE9178',
    tokenInvalid: 'var(--vscode-errorForeground, #F85149)',
};
var lightTokenColors = {
    tokenFunction: '#795E26',
    tokenType: '#267f99',
    tokenKeyword: '#0000FF',
    tokenPunctuation: lightColors.editorForeground,
    tokenParens: '#AF00DB',
    tokenVarName: '#001080',
    tokenConstant: '#0070C1',
    tokenPropery: '#0451a5',
    tokenComment: '#008000',
    tokenString: '#A31515',
    tokenInvalid: 'var(--vscode-errorForeground, #F85149)',
};

var defaultTokenColors = {};
for (const key of Object.getOwnPropertyNames(darkTokenColors)) {
    defaultTokenColors[key] = `light-dark(${lightTokenColors[key]}, ${darkTokenColors[key]})`;
}

function completionIcon(color, path) {
    const svg = btoa(`<svg width="16px" height="16px" viewBox="0 0 16 16" fill="${color}" xmlns="http://www.w3.org/2000/svg">${path}</svg>`);
    return `url("data:image/svg+xml;base64,${svg}")`;
}

function vscodeTheme(
    {
        editorForeground,
        editorBackground,
        editorSelectionBackground,
        editorLineNumberForeground,
        editorMatchBackground,
        editorSelectedMatchBackground,
        panelBackground,
        panelForeground,
        panelBorder,
        focusedOutline,
        tokenOperator,
        tokenValue,
        tokenLiteral,
        tokenType,
    }
) {
    return {
        '&': {
            color: editorForeground,
            backgroundColor: editorBackground,
        },
        '.cm-content': {
            caretColor: editorForeground,
        },
        '.cm-line': {
            padding: '0 6px',
        },
        '&.cm-focused .cm-cursor, .cm-dropCursor': {
            borderLeftColor: editorForeground,
            borderLeftWidth: '2px',
        },
        '&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection': {
            color: 'unset',
            backgroundColor: editorSelectionBackground,
        },
        /* Panel styles */
        '.cm-panels': {
            backgroundColor: panelBackground,
            color: panelForeground,
            zIndex: 1,
        },
        '.cm-panels.cm-panels-top': {
            borderBottom: '1px solid '.concat(panelBorder),
        },
        '.cm-panels.cm-panels-bottom': {
            borderTop: '1px solid '.concat(panelBorder),
        },
        /* Active line and search matching */
        '.cm-searchMatch': {
            backgroundColor: editorMatchBackground,
            outline: "1px solid ".concat(editorMatchBackground),
        },
        '.cm-searchMatch.cm-searchMatch-selected': {
            backgroundColor: editorSelectedMatchBackground,
            outlineColor: editorSelectedMatchBackground,
        },
        '.cm-activeLine': {
            backgroundColor: editorSelectionBackground,
        },
        '.cm-selectionMatch': {
            backgroundColor: editorSelectionBackground,
        },
        '&.cm-focused .cm-matchingBracket, &.cm-focused .cm-nonmatchingBracket': {
            outline: "1px solid ".concat(editorSelectionBackground),
        },
        '&.cm-focused .cm-matchingBracket': {
            backgroundColor: editorSelectionBackground,
        },
        /* Gutter styles */
        '.cm-gutters': {
            backgroundColor: editorBackground,
            color: editorLineNumberForeground,
            border: 'none',
            userSelect: 'none',
        },
        '.cm-activeLineGutter': {
            color: editorForeground,
            backgroundColor: editorBackground,
        },
        '.cm-foldPlaceholder': {
            backgroundColor: panelBackground,
            border: '1px solid '.concat(panelBorder),
            borderRadius: '4px',
            color: panelForeground,
        },
        /* Tooltip + completion styles */
        '.cm-tooltip': {
            border: '1px solid '.concat(panelBorder),
            borderRadius: '4px',
            backgroundColor: panelBackground,
            color: panelForeground,
        },
        '.cm-tooltip .cm-tooltip-arrow:before': {
            display: 'none',
        },
        '.cm-tooltip .cm-tooltip-arrow:after': {
            display: 'none',
        },
        /* Autocomplete */
        '.cm-tooltip.cm-tooltip-autocomplete': {
            overflow: 'hidden',
            '& > ul > li[aria-selected]': {
                backgroundColor: editorSelectionBackground,
                color: panelForeground,
            },
            '& > ul > li': {
                display: 'flex',
                alignItems: 'center',
                maxWidth: '400px',
                padding: '1px 0',
            },
        },
        '.cm-completionLabel': {
            margin: '0',
            flexShrink: '1',
            color: editorForeground,
        },
        '.cm-completionLabel .cm-completionMatchedText': {
            fontWeight: 'bold',
            textDecoration: 'underline',
        },
        '.cm-completionDetail': {
            margin: '.3em 0 0 .5em',
            flexShrink: '3',
            color: editorLineNumberForeground,
            fontSize: '0.8em',
            overflow: 'hidden',
            textOverflow: 'ellipsis',
        },
        /* Autompletion Icons (Codicons) */
        '.cm-completionIcon': {
            width: '16px',
            height: '16px',
            margin: '0',
            flexShrink: '0',
            backgroundSize: '16px 16px',
            backgroundPosition: 'center',
            backgroundRepeat: 'no-repeat',
            opacity: '1',
        },
        '.cm-completionIcon:after': {
            content: 'none',
        },
        '.cm-completionIcon-class': {
            backgroundImage: completionIcon(tokenType, '<path d="M11.34 9.71h.71l2.67-2.67v-.71L13.38 5h-.7l-1.82 1.81h-5V5.56l1.86-1.85V3l-2-2H5L1 5v.71l2 2h.71l1.14-1.15v5.79l.5.5H10v.52l1.33 1.34h.71l2.67-2.67v-.71L13.37 10h-.7l-1.86 1.85h-5v-4H10v.48l1.34 1.38zm1.69-3.65l.63.63-2 2-.63-.63 2-2zm0 5l.63.63-2 2-.63-.63 2-2zM3.35 6.65l-1.29-1.3 3.29-3.29 1.3 1.29-3.3 3.3z"></path>'),
        },
        '.cm-completionIcon-constant': {
            backgroundImage: completionIcon(tokenLiteral, '<path fill-rule="evenodd" clip-rule="evenodd" d="M4 6h8v1H4V6zm8 3H4v1h8V9z"></path><path fill-rule="evenodd" clip-rule="evenodd" d="M1 4l1-1h12l1 1v8l-1 1H2l-1-1V4zm1 0v8h12V4H2z"></path>'),
        },
        '.cm-completionIcon-enum': {
            backgroundImage: completionIcon(tokenValue, '<path fill-rule="evenodd" clip-rule="evenodd" d="M14 2H8L7 3v3h1V3h6v5h-4v1h4l1-1V3l-1-1zM9 6h4v1H9.41L9 6.59V6zM7 7H2L1 8v5l1 1h6l1-1V8L8 7H7zm1 6H2V8h6v5zM3 9h4v1H3V9zm0 2h4v1H3v-1zm6-7h4v1H9V4z"></path>'),
        },
        '.cm-completionIcon-function': {
            backgroundImage: completionIcon(tokenOperator, '<path d="M14.45 4.5l-5-2.5h-.9l-7 3.5-.55.89v4.5l.55.9 5 2.5h.9l7-3.5.55-.9v-4.5l-.55-.89zm-8 8.64l-4.5-2.25V7.17l4.5 2v3.97zm.5-4.8L2.29 6.23l6.66-3.34 4.67 2.34-6.67 3.11zm7 1.55l-6.5 3.25V9.21l6.5-3v3.68z"></path>'),
        },
        '.cm-completionIcon-interface': {
            backgroundImage: completionIcon(tokenType, '<path d="M11.496 4a3.49 3.49 0 0 0-3.46 3h-3.1a2 2 0 1 0 0 1h3.1a3.5 3.5 0 1 0 3.46-4zm0 6a2.5 2.5 0 1 1 0-5 2.5 2.5 0 0 1 0 5z"></path>'),
        },
        '.cm-completionIcon-keyword': {
            backgroundImage: completionIcon(editorForeground, '<path d="M15 4h-5V3h5v1zm-1 3h-2v1h2V7zm-4 0H1v1h9V7zm2 6H1v1h11v-1zm-5-3H1v1h6v-1zm8 0h-5v1h5v-1zM8 2v3H1V2h7zM7 3H2v1h5V3z"></path>'),
        },
        '.cm-completionIcon-method': {
            backgroundImage: completionIcon(tokenOperator, '<path d="M13.51 4l-5-3h-1l-5 3-.49.86v6l.49.85 5 3h1l5-3 .49-.85v-6L13.51 4zm-6 9.56l-4.5-2.7V5.7l4.5 2.45v5.41zM3.27 4.7l4.74-2.84 4.74 2.84-4.74 2.59L3.27 4.7zm9.74 6.16l-4.5 2.7V8.15l4.5-2.45v5.16z"></path>'),
        },
        '.cm-completionIcon-namespace': {
            backgroundImage: completionIcon(editorForeground, '<path fill-rule="evenodd" clip-rule="evenodd" d="M6 2.984V2h-.09c-.313 0-.616.062-.909.185a2.33 2.33 0 0 0-.775.53 2.23 2.23 0 0 0-.493.753v.001a3.542 3.542 0 0 0-.198.83v.002a6.08 6.08 0 0 0-.024.863c.012.29.018.58.018.869 0 .203-.04.393-.117.572v.001a1.504 1.504 0 0 1-.765.787 1.376 1.376 0 0 1-.558.115H2v.984h.09c.195 0 .38.04.556.121l.001.001c.178.078.329.184.455.318l.002.002c.13.13.233.285.307.465l.001.002c.078.18.117.368.117.566 0 .29-.006.58-.018.869-.012.296-.004.585.024.87v.001c.033.283.099.558.197.824v.001c.106.273.271.524.494.753.223.23.482.407.775.53.293.123.596.185.91.185H6v-.984h-.09c-.199 0-.387-.038-.562-.115a1.613 1.613 0 0 1-.457-.32 1.659 1.659 0 0 1-.309-.467c-.074-.18-.11-.37-.11-.573 0-.228.003-.453.011-.672.008-.228.008-.45 0-.665a4.639 4.639 0 0 0-.055-.64 2.682 2.682 0 0 0-.168-.609A2.284 2.284 0 0 0 3.522 8a2.284 2.284 0 0 0 .738-.955c.08-.192.135-.393.168-.602.033-.21.051-.423.055-.64.008-.22.008-.442 0-.666-.008-.224-.012-.45-.012-.678a1.47 1.47 0 0 1 .877-1.354 1.33 1.33 0 0 1 .563-.121H6zm4 10.032V14h.09c.313 0 .616-.062.909-.185.293-.123.552-.3.775-.53.223-.23.388-.48.493-.753v-.001c.1-.266.165-.543.198-.83v-.002c.028-.28.036-.567.024-.863-.012-.29-.018-.58-.018-.869 0-.203.04-.393.117-.572v-.001a1.504 1.504 0 0 1 .765-.787c.176-.077.362-.115.558-.115H14v-.984h-.09c-.195 0-.38-.04-.556-.121l-.001-.001a1.376 1.376 0 0 1-.455-.318l-.002-.002a1.414 1.414 0 0 1-.307-.465l-.001-.002a1.405 1.405 0 0 1-.117-.566c0-.29.006-.58.018-.869a6.19 6.19 0 0 0-.024-.87v-.001a3.542 3.542 0 0 0-.197-.824v-.001a2.23 2.23 0 0 0-.494-.753 2.33 2.33 0 0 0-.775-.53 2.325 2.325 0 0 0-.91-.185H10v.984h.09c.2 0 .386.038.562.115.174.082.326.188.457.32.127.134.23.29.309.467.074.18.11.37.11.573 0 .228-.003.452-.011.672-.008.228-.008.45 0 .665.004.222.022.435.055.64.033.214.089.416.168.609a2.282 2.282 0 0 0 .738.955 2.282 2.282 0 0 0-.738.955 2.7 2.7 0 0 0-.168.602c-.033.21-.051.423-.055.64-.008.22-.008.442 0 .666.008.224.012.45.012.678a1.47 1.47 0 0 1-.42 1.035 1.466 1.466 0 0 1-.457.319 1.33 1.33 0 0 1-.563.121H10z"></path>'),
        },
        '.cm-completionIcon-property': {
            backgroundImage: completionIcon(tokenValue, '<path d="M2.807 14.975a1.75 1.75 0 0 1-1.255-.556 1.684 1.684 0 0 1-.544-1.1A1.72 1.72 0 0 1 1.36 12.1c1.208-1.27 3.587-3.65 5.318-5.345a4.257 4.257 0 0 1 .048-3.078 4.095 4.095 0 0 1 1.665-1.969 4.259 4.259 0 0 1 4.04-.36l.617.268-2.866 2.951 1.255 1.259 2.944-2.877.267.619a4.295 4.295 0 0 1 .04 3.311 4.198 4.198 0 0 1-.923 1.392 4.27 4.27 0 0 1-.743.581 4.217 4.217 0 0 1-3.812.446c-1.098 1.112-3.84 3.872-5.32 5.254a1.63 1.63 0 0 1-1.084.423zm7.938-13.047a3.32 3.32 0 0 0-1.849.557c-.213.13-.412.284-.591.458a3.321 3.321 0 0 0-.657 3.733l.135.297-.233.227c-1.738 1.697-4.269 4.22-5.485 5.504a.805.805 0 0 0 .132 1.05.911.911 0 0 0 .298.22c.1.044.209.069.319.072a.694.694 0 0 0 .45-.181c1.573-1.469 4.612-4.539 5.504-5.44l.23-.232.294.135a3.286 3.286 0 0 0 3.225-.254 3.33 3.33 0 0 0 .591-.464 3.28 3.28 0 0 0 .964-2.358c0-.215-.021-.43-.064-.642L11.43 7.125 8.879 4.578l2.515-2.59a3.286 3.286 0 0 0-.65-.06z"></path>'),
        },
        '.cm-completionIcon-text': {
            backgroundImage: completionIcon(editorForeground, '<path fill-rule="evenodd" clip-rule="evenodd" d="M2 2L1 3v9l1 1h12l1-1V3l-1-1H2zm0 10V3h12v9H2zm3.356-3.07H6V7.22C6 6.408 5.685 6 5.056 6c-.135 0-.285.024-.45.073a1.444 1.444 0 0 0-.388.167v.665c.237-.203.487-.304.75-.304.261 0 .392.156.392.469l-.6.103c-.506.086-.76.406-.76.961 0 .263.061.473.183.631A.61.61 0 0 0 4.69 9c.29 0 .509-.16.657-.48h.009v.41zm.004-1.355v.193a.75.75 0 0 1-.12.436.368.368 0 0 1-.313.17.276.276 0 0 1-.22-.095.38.38 0 0 1-.08-.248c0-.222.11-.351.332-.389l.4-.067zM7.6 8.626h-.007v.31H7V5h.593v1.677h.008c.146-.31.355-.465.625-.465.248 0 .44.118.573.353.134.236.201.557.201.966 0 .443-.078.798-.235 1.067C8.61 8.866 8.4 9 8.138 9c-.237 0-.416-.125-.537-.374zm-.016-1.121v.272a.78.78 0 0 0 .107.426c.071.113.163.169.274.169.135 0 .24-.072.314-.216.075-.145.113-.35.113-.615 0-.22-.035-.39-.104-.514-.067-.124-.164-.187-.29-.187-.12 0-.219.062-.298.185a.887.887 0 0 0-.116.48zM11.262 9c.321 0 .567-.058.738-.173v-.71a.9.9 0 0 1-.552.207.619.619 0 0 1-.5-.215c-.12-.145-.181-.345-.181-.598 0-.26.063-.464.189-.612a.644.644 0 0 1 .516-.223c.194 0 .37.069.528.207v-.749c-.129-.09-.338-.134-.626-.134-.417 0-.751.14-1.001.422-.249.28-.373.662-.373 1.148 0 .42.116.764.349 1.03.232.267.537.4.913.4z"></path>'),
        },
        '.cm-completionIcon-type': {
            backgroundImage: completionIcon(tokenType, '<path fill-rule="evenodd" clip-rule="evenodd" d="M11 6h-1v-.5a.5.5 0 0 0-.5-.5H8.479v5.5a.5.5 0 0 0 .5.5h.5v1h-3v-1h.5a.5.5 0 0 0 .5-.5V5H6.5a.5.5 0 0 0-.5.5V6H5V4h6v2zm2.914 2.048l-1.462-1.462.707-.707 1.816 1.816v.707l-1.768 1.767-.707-.707 1.414-1.414zM3.548 9.462L2.086 8 3.5 6.586l-.707-.707-1.768 1.767v.708l1.816 1.815.707-.707z"></path>'),
        },
        '.cm-completionIcon-variable': {
            backgroundImage: completionIcon(tokenValue, '<path fill-rule="evenodd" clip-rule="evenodd" d="M2 5h2V4H1.5l-.5.5v8l.5.5H4v-1H2V5zm12.5-1H12v1h2v7h-2v1h2.5l.5-.5v-8l-.5-.5zm-2.74 2.57L12 7v2.51l-.3.45-4.5 2h-.46l-2.5-1.5-.24-.43v-2.5l.3-.46 4.5-2h.46l2.5 1.5zM5 9.71l1.5.9V9.28L5 8.38v1.33zm.58-2.15l1.45.87 3.39-1.5-1.45-.87-3.39 1.5zm1.95 3.17l3.5-1.56v-1.4l-3.5 1.55v1.41z"></path>'),
        },
        /* Search panel styles */
        '.cm-search': {
            display: 'flex',
            alignItems: 'center',
            userSelect: 'none',
        },
        '.cm-search .cm-textfield': {
            borderRadius: '2px',
            border: '1px solid '.concat(panelBorder),
            backgroundColor: editorBackground,
        },
        '.cm-search .cm-textfield:focus': {
            borderRadius: '2px',
            border: '1px solid '.concat(focusedOutline),
            outline: 'none',
        },
        '.cm-search .cm-button': {
            padding: '2px 4px',
            backgroundImage: 'none',
            background: panelBorder,
            borderRadius: '2px',
            border: '1px solid '.concat(panelBorder),
            color: panelForeground,
        },
        '.cm-search .cm-button:active': {
            borderColor: focusedOutline,
            background: panelBorder,
        },
        '.cm-search label': {
            display: 'inline-flex',
            flexDirection: 'row',
            alignItems: 'center',
        },
    }
}

function vscodeHighlightStyle(
    {
        editorForeground,
    },
    {
        tokenFunction,
        tokenType,
        tokenKeyword,
        tokenPunctuation,
        tokenParens,
        tokenVarName,
        tokenConstant,
        tokenPropery,
        tokenComment,
        tokenString,
        tokenInvalid,
    }) {
    return function () {
        var _highlight = codemirror.Lezer_highlight;
        return [{
            tag: _highlight.tags.keyword,
            color: tokenKeyword
        }, {
            tag: [_highlight.tags.name, _highlight.tags.deleted, _highlight.tags.character, _highlight.tags.propertyName, _highlight.tags.macroName],
            color: tokenPropery
        }, {
            tag: [_highlight.tags.variableName],
            color: tokenVarName
        }, {
            tag: [_highlight.tags["function"](_highlight.tags.variableName)],
            color: tokenFunction
        }, {
            tag: [_highlight.tags.labelName],
            color: tokenConstant
        }, {
            tag: [_highlight.tags.color, _highlight.tags.constant(_highlight.tags.name), _highlight.tags.standard(_highlight.tags.name)],
            color: tokenPropery
        }, {
            tag: [_highlight.tags.definition(_highlight.tags.name), _highlight.tags.separator],
            color: tokenConstant
        }, {
            tag: [_highlight.tags.brace],
            color: tokenPunctuation
        }, {
            tag: [_highlight.tags.annotation],
            color: tokenInvalid
        }, {
            tag: [_highlight.tags.number, _highlight.tags.changed, _highlight.tags.annotation, _highlight.tags.modifier, _highlight.tags.self, _highlight.tags.namespace],
            color: tokenConstant
        }, {
            tag: [_highlight.tags.typeName, _highlight.tags.className],
            color: tokenType
        }, {
            tag: [_highlight.tags.operator, _highlight.tags.operatorKeyword],
            color: tokenPunctuation
        }, {
            tag: [_highlight.tags.tagName],
            color: tokenPropery
        }, {
            tag: [_highlight.tags.squareBracket],
            color: tokenParens
        }, {
            tag: [_highlight.tags.angleBracket],
            color: tokenParens
        }, {
            tag: [_highlight.tags.attributeName],
            color: tokenVarName
        }, {
            tag: [_highlight.tags.regexp],
            color: tokenParens
        }, {
            tag: [_highlight.tags.quote],
            color: tokenType
        }, {
            tag: [_highlight.tags.string],
            color: tokenString
        }, {
            tag: _highlight.tags.link,
            color: tokenComment
        }, {
            tag: [_highlight.tags.url, _highlight.tags.escape, _highlight.tags.special(_highlight.tags.string)],
            color: tokenFunction,
            textDecoration: 'underline',
            textUnderlinePosition: 'under'
        }, {
            tag: [_highlight.tags.meta],
            color: tokenType
        }, {
            tag: [_highlight.tags.comment],
            color: tokenComment,
        }, {
            tag: _highlight.tags.strong,
            fontWeight: 'bold',
            color: tokenVarName
        }, {
            tag: _highlight.tags.emphasis,
            fontStyle: 'italic',
            color: tokenVarName
        }, {
            tag: _highlight.tags.strikethrough,
            textDecoration: 'line-through'
        }, {
            tag: _highlight.tags.heading,
            fontWeight: 'bold',
            color: editorForeground
        }, {
            tag: _highlight.tags.special(_highlight.tags.heading1),
            fontWeight: 'bold',
            color: tokenConstant
        }, {
            tag: _highlight.tags.heading1,
            fontWeight: 'bold',
            color: tokenConstant
        }, {
            tag: [_highlight.tags.heading2, _highlight.tags.heading3, _highlight.tags.heading4],
            fontWeight: 'bold',
            color: tokenType
        }, {
            tag: [_highlight.tags.heading5, _highlight.tags.heading6],
            color: tokenType
        }, {
            tag: [_highlight.tags.atom, _highlight.tags.bool, _highlight.tags.special(_highlight.tags.variableName)],
            color: tokenFunction
        }, {
            tag: [_highlight.tags.processingInstruction, _highlight.tags.inserted],
            color: tokenFunction
        }, {
            tag: [_highlight.tags.contentSeparator],
            color: tokenPunctuation
        }, {
            tag: _highlight.tags.invalid,
            color: editorForeground,
            borderBottom: "1px dotted ".concat(tokenInvalid)
        }]
    };
}

/* Expose the dark and light themes to JS. */
globalThis.cm6_themes_vscodeDarkTheme = vscodeTheme(darkColors);
globalThis.cm6_themes_vscodeDarkHighlightStyle = vscodeHighlightStyle(darkColors, darkTokenColors);
globalThis.cm6_themes_vscodeLightTheme = vscodeTheme(lightColors);
globalThis.cm6_themes_vscodeLightHighlightStyle = vscodeHighlightStyle(lightColors, lightTokenColors);
globalThis.cm6_themes_vscodeDefaultTheme = vscodeTheme(defaultColors);
globalThis.cm6_themes_vscodeDefaultHighlightStyle = vscodeHighlightStyle(defaultColors, defaultTokenColors);
