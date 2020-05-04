# Fugit

A small CLI to launch notifications at a given time

```bash
% fugit tea in 3min30s # The optimal brewing time for tea
Alert will trigger in 3 minutes.

% fugit "look at the oven" 1.5h # Remind you to look at the oven
Alert will trigger in 1 hour and 30 minutes.
```

It has a mildly clever parser that understands durations in english-ish.
You can also feed it ISO8601 if you are Totally A Human.
Long (>1h) timers will persist (and properly notify you on wakeup/boot/etc).

It can also record commands that you launch often:

```bash
% fugit record tea 3.5m # Record a tea alert

% fugit list
tea:
  message: tea
  duration: 3 minutes and 30 seconds

% fugit show tea
message: tea
duration: 3 minutes and 30 seconds

% fugit tea
Alert will trigger in 3 minutes and 30 seconds.
```

It has a nice editable configuration file under `$HOME/.config/fugit/config`.

## Compatibility

It might work under my linux setup. It probably doesn't work anywhere else.
It uses `notify-send` and either `sleep` (for short durations) or `systemd` (for long durations). Additional notification and/or delay implementations are welcome.
