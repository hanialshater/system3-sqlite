SELECT date('2026-10-25', '+1 month', 'start of month'), date('2024-02-28', '+1 day'), date('2023-02-28', '+1 day');
SELECT strftime('%Y week %W, day %j', '2026-10-25'), julianday('2026-12-25') - julianday('2026-10-25');
SELECT time('12:34:56', '+90 minutes'), datetime(0, 'unixepoch'), strftime('%s', '2000-01-01');
