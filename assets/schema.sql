CREATE TABLE runs (
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	concurrency INTEGER NOT NULL,
	test_match TEXT,
	group_name TEXT,
	created DATETIME
);

CREATE TABLE rollups (
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	run_id INTEGER NOT NULL,
	hits INTEGER,
	max_time REAL,
	mean_time REAL,
	min_time REAL,
	total_time REAL,
	var_time REAL,
	hits_2xx INTEGER,
	hits_4xx INTEGER,
	hits_5xx INTEGER,
	failed INTEGER
);

CREATE TABLE pages (
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	run_id INTEGER NOT NULL,
	page_url TEXT,
	hits INTEGER,
	max_time REAL,
	mean_time REAL,
	min_time REAL,
	total_time REAL,
	var_time REAL,
	hits_2xx INTEGER,
	hits_4xx INTEGER,
	hits_5xx INTEGER,
	failed INTEGER
);
