CREATE TABLE stock(sku TEXT PRIMARY KEY, qty INT NOT NULL);
INSERT INTO stock VALUES ('apple', 5), ('pear', 2);
INSERT INTO stock VALUES ('apple', 3) ON CONFLICT(sku) DO UPDATE SET qty = qty + excluded.qty RETURNING sku, qty;
INSERT INTO stock VALUES ('kiwi', 7) ON CONFLICT DO NOTHING RETURNING *;
UPDATE stock SET qty = qty - 1 WHERE qty > 2 RETURNING sku, qty;
SELECT * FROM stock ORDER BY sku;
