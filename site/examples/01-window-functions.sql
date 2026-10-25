CREATE TABLE sales(region TEXT, month INT, amount REAL);
INSERT INTO sales VALUES ('north',1,120),('north',2,90),('north',3,150),('south',1,80),('south',2,110),('south',3,95);
SELECT region, month, amount, sum(amount) OVER (PARTITION BY region ORDER BY month) AS running, rank() OVER (PARTITION BY region ORDER BY amount DESC) AS rnk FROM sales ORDER BY region, month;
SELECT region, month, amount, round(avg(amount) OVER (ORDER BY month, region ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING), 2) AS smooth FROM sales ORDER BY month, region;
