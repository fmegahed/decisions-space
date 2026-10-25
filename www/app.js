// Client-side behaviour: the tabs, the Help switch, the intro, the Samples
// menu, the video dialog, the Copy button, the colours of the R code and the
// run state. The script sits at the end of the page, so the elements exist.
// Every access to browser storage is in try/catch (a private window or a
// blocked site can refuse it).
(function() {
  'use strict';

  function readStore(store, key) {
    try { return window[store].getItem(key); } catch (e) { return null; }
  }
  function writeStore(store, key, value) {
    try { window[store].setItem(key, value); } catch (e) { /* storage refused */ }
  }
  function byId(id) { return document.getElementById(id); }

  // ---- Tabs: Build, About, Record ----------------------------------------

  var TABS = ['build', 'about', 'record'];

  function showTab(name, moveFocus) {
    TABS.forEach(function(t) {
      var tab = byId('tab-' + t);
      var panel = byId('panel-' + t);
      if (!tab || !panel) return;
      var on = (t === name);
      tab.setAttribute('aria-selected', on ? 'true' : 'false');
      tab.setAttribute('tabindex', on ? '0' : '-1');
      panel.hidden = !on;
      if (on && moveFocus) tab.focus();
    });
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('tab', name);
  }

  function onTabKey(event) {
    var index = TABS.indexOf(event.currentTarget.id.replace('tab-', ''));
    var next = null;
    if (event.key === 'ArrowRight') next = TABS[(index + 1) % TABS.length];
    else if (event.key === 'ArrowLeft') next = TABS[(index + TABS.length - 1) % TABS.length];
    else if (event.key === 'Home') next = TABS[0];
    else if (event.key === 'End') next = TABS[TABS.length - 1];
    if (next !== null) {
      event.preventDefault();
      showTab(next, true);
    }
  }

  TABS.forEach(function(t) {
    var tab = byId('tab-' + t);
    if (!tab) return;
    tab.addEventListener('click', function() { showTab(t, false); });
    tab.addEventListener('keydown', onTabKey);
  });

  // ---- Help switch: hides the help lines and notes; kept for the session --

  var helpSwitch = byId('help_on');
  function applyHelp() { document.body.classList.toggle('help-off', !helpSwitch.checked); }
  if (helpSwitch) {
    if (readStore('sessionStorage', 'decisions-help') === 'off') helpSwitch.checked = false;
    applyHelp();
    helpSwitch.addEventListener('change', function() {
      applyHelp();
      writeStore('sessionStorage', 'decisions-help', helpSwitch.checked ? 'on' : 'off');
    });
  }

  // ---- Intro: open on the first visit; Hide closes it for this browser ----

  var intro = byId('intro');
  if (intro && readStore('localStorage', 'decisions-intro') === 'hidden') intro.hidden = true;
  var introHide = byId('intro_hide');
  if (introHide) {
    introHide.addEventListener('click', function(event) {
      event.preventDefault();
      intro.hidden = true;
      writeStore('localStorage', 'decisions-intro', 'hidden');
      var title = byId('answers_title');
      if (title) title.focus();
    });
  }

  // ---- Samples menu: closes on a choice, on Escape and on a click outside --

  function closeMenu(menu, moveFocus) {
    if (!menu.open) return;
    menu.open = false;
    if (moveFocus) menu.querySelector('summary').focus();
  }

  document.addEventListener('click', function(event) {
    document.querySelectorAll('details.menu[open]').forEach(function(menu) {
      if (!menu.contains(event.target)) closeMenu(menu, false);
      else if (event.target.closest('.menu-item')) closeMenu(menu, true);
    });
  });

  document.addEventListener('keydown', function(event) {
    if (event.key !== 'Escape') return;
    document.querySelectorAll('details.menu[open]').forEach(function(menu) {
      closeMenu(menu, menu.contains(document.activeElement));
    });
  });

  // ---- Video dialog (only when the page has one) ---------------------------
  // The embed address is set when the dialog opens and cleared when it
  // closes, so nothing loads before a click and the video stops on close.
  // Escape closes the dialog; Tab stays in the dialog.

  var dialog = byId('video_dialog');
  var openVideo = byId('open_video');
  if (dialog && openVideo && dialog.showModal) {
    var frame = byId('video_frame');
    var closeButton = byId('close_video');
    var lastFocused = null;

    openVideo.addEventListener('click', function() {
      lastFocused = document.activeElement;
      frame.setAttribute('src', frame.getAttribute('data-src'));
      dialog.showModal();
      closeButton.focus();
    });
    closeButton.addEventListener('click', function() { dialog.close(); });
    dialog.addEventListener('click', function(event) {
      if (event.target === dialog) dialog.close();
    });
    dialog.addEventListener('close', function() {
      frame.setAttribute('src', 'about:blank');
      if (lastFocused && lastFocused.focus) lastFocused.focus();
    });
    dialog.addEventListener('keydown', function(event) {
      if (event.key !== 'Tab') return;
      if (event.shiftKey && document.activeElement === closeButton) {
        event.preventDefault();
        frame.focus();
      } else if (!event.shiftKey && document.activeElement === frame) {
        event.preventDefault();
        closeButton.focus();
      }
    });
  }

  // ---- Copy button of the R code -------------------------------------------
  // The code output sits in the R code card, which is closed when the page
  // opens. Shiny renders a hidden output only when it shows, so the code can
  // be empty. Then Copy opens the card and copies when the code arrives (see
  // the shiny:value handler below), or says that there is nothing to copy
  // yet. Copy never says "Copied" for an empty text.

  var pendingCopy = null;
  var pendingTimer = null;

  // Show a message on the button and in the live region for 2 seconds
  function sayCopy(button, message) {
    var label = button.querySelector('.copy-label');
    var status = byId('copy_status');
    if (label) label.textContent = message;
    if (status) status.textContent = message;
    window.setTimeout(function() {
      if (label) label.textContent = 'Copy';
      if (status) status.textContent = '';
    }, 2000);
  }

  function writeCopy(button, text) {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(
        function() { sayCopy(button, 'Copied'); },
        function() { sayCopy(button, 'Copy failed. Select the code and copy it.'); }
      );
    } else {
      sayCopy(button, 'Copy is not available. Select the code and copy it.');
    }
  }

  function copyCode(button) {
    var target = byId(button.getAttribute('data-target'));
    if (!target) return;
    var text = target.textContent;
    if (!text) {
      var details = target.closest('details');
      if (details && !details.open) {
        // Open the card; the copy waits for the code, at most 5 seconds
        pendingCopy = button;
        details.open = true;
        window.clearTimeout(pendingTimer);
        pendingTimer = window.setTimeout(function() {
          if (pendingCopy !== button) return;
          pendingCopy = null;
          sayCopy(button, 'Nothing to copy yet.');
        }, 5000);
      } else {
        sayCopy(button, 'Nothing to copy yet.');
      }
      return;
    }
    writeCopy(button, text);
  }

  document.addEventListener('click', function(event) {
    var button = event.target.closest ? event.target.closest('.copy-btn') : null;
    if (button) copyCode(button);
  });

  // ---- Colours of the R code -----------------------------------------------
  // The server sends the code as plain text. The text is escaped first, then
  // comments, strings and function names get a span. Copy reads textContent,
  // so it copies the plain code.

  var TOKEN = /(#[^\n]*)|("(?:[^"\\\n]|\\.)*")|([A-Za-z.][A-Za-z0-9._]*)(?=\()/g;

  function escapeHtml(text) {
    return text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  function highlightR(code) {
    var html = '';
    var last = 0;
    var match;
    TOKEN.lastIndex = 0;
    while ((match = TOKEN.exec(code)) !== null) {
      html += escapeHtml(code.slice(last, match.index));
      var kind = match[1] ? 'c' : (match[2] ? 's' : 'f');
      html += '<span class="' + kind + '">' + escapeHtml(match[0]) + '</span>';
      last = TOKEN.lastIndex;
    }
    return html + escapeHtml(code.slice(last));
  }

  if (window.jQuery) {
    window.jQuery(document).on('shiny:value', function(event) {
      if (event.name !== 'code' || typeof event.value !== 'string') return;
      event.preventDefault();
      byId('code').innerHTML = highlightR(event.value);
      // A Copy that waited for the code runs now
      if (pendingCopy) {
        var button = pendingCopy;
        pendingCopy = null;
        window.clearTimeout(pendingTimer);
        if (event.value) writeCopy(button, event.value);
        else sayCopy(button, 'Nothing to copy yet.');
      }
    });
  }

  // ---- Run state -------------------------------------------------------------
  // The server sends session$sendCustomMessage("run-state", TRUE) when a run
  // starts and FALSE when it ends: Stop replaces Try on 5 and Run all.

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('run-state', function(running) {
      document.body.classList.toggle('is-running', !!running);
    });
  }
})();
