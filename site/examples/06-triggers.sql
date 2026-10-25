CREATE TABLE account(id INTEGER PRIMARY KEY, balance INT);
CREATE TABLE audit(id INTEGER PRIMARY KEY, account INT, old INT, new INT);
CREATE TRIGGER log_change AFTER UPDATE OF balance ON account BEGIN INSERT INTO audit(account, old, new) VALUES (old.id, old.balance, new.balance); END;
INSERT INTO account VALUES (1, 100), (2, 50);
UPDATE account SET balance = balance - 30 WHERE id = 1;
UPDATE account SET balance = balance + 30 WHERE id = 2;
SELECT * FROM audit;
