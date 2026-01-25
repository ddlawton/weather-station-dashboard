-- PostgreSQL schema and seed data for Tempest Listener (weatherdata database)

-- Table: obs_st
CREATE TABLE obs_st (
    id SERIAL PRIMARY KEY,
    timestamp TIMESTAMPTZ NOT NULL,
    station_id TEXT NOT NULL,
    wind_lull REAL,
    wind_avg REAL,
    wind_gust REAL,
    wind_dir REAL,
    wind_interval INTEGER,
    pressure REAL,
    air_temp REAL,
    humidity REAL,
    illuminance REAL,
    uv REAL,
    solar_rad REAL,
    precip REAL,
    precip_type INTEGER,
    lightning_dist REAL,
    lightning_count INTEGER,
    battery REAL,
    report_interval INTEGER
);
CREATE INDEX idx_obs_st_station_time ON obs_st (station_id, timestamp DESC);
INSERT INTO obs_st (timestamp, station_id, wind_lull, wind_avg, wind_gust, wind_dir, wind_interval, pressure, air_temp, humidity, illuminance, uv, solar_rad, precip, precip_type, lightning_dist, lightning_count, battery, report_interval)
VALUES
  (NOW(), 'ST-00175439', 2.0, 3.2, 5.0, 180, 60, 1012.3, 20.5, 55.0, 1000, 3.5, 500, 0.5, 1, 10.0, 0, 2.8, 60),
  (NOW() - INTERVAL '1 hour', 'ST-00175439', 1.5, 2.8, 4.0, 190, 60, 1011.8, 19.0, 60.0, 900, 2.8, 400, 0.0, 0, 12.0, 1, 2.7, 60);

-- Table: rapid_wind_1min
CREATE TABLE rapid_wind_1min (
    id SERIAL PRIMARY KEY,
    minute_timestamp TIMESTAMPTZ NOT NULL,
    station_id TEXT NOT NULL,
    wind_speed_avg REAL,
    wind_speed_max REAL,
    wind_speed_min REAL,
    wind_dir_avg REAL,
    sample_count INTEGER
);
CREATE INDEX idx_rapid_wind_1min_station_time ON rapid_wind_1min (station_id, minute_timestamp DESC);
INSERT INTO rapid_wind_1min (minute_timestamp, station_id, wind_speed_avg, wind_speed_max, wind_speed_min, wind_dir_avg, sample_count)
VALUES
  (NOW(), 'ST-00175439', 3.2, 5.0, 2.0, 180, 60),
  (NOW() - INTERVAL '1 hour', 'ST-00175439', 2.8, 4.0, 1.5, 190, 60);

-- Table: device_status
CREATE TABLE device_status (
    id SERIAL PRIMARY KEY,
    timestamp TIMESTAMPTZ NOT NULL,
    station_id TEXT NOT NULL,
    uptime INTEGER,
    voltage REAL,
    firmware_revision INTEGER,
    rssi INTEGER,
    hub_rssi INTEGER,
    sensor_status INTEGER,
    debug INTEGER
);
CREATE INDEX idx_device_status_station_time ON device_status (station_id, timestamp DESC);
INSERT INTO device_status (timestamp, station_id, uptime, voltage, firmware_revision, rssi, hub_rssi, sensor_status, debug)
VALUES
  (NOW(), 'ST-00175439', 100000, 2.8, 123, -70, -65, 0, 0),
  (NOW() - INTERVAL '1 hour', 'ST-00175439', 99500, 2.7, 122, -72, -67, 0, 0);

-- Table: hub_status
CREATE TABLE hub_status (
    id SERIAL PRIMARY KEY,
    timestamp TIMESTAMPTZ NOT NULL,
    hub_sn TEXT NOT NULL,
    uptime INTEGER,
    firmware_revision TEXT,
    rssi INTEGER,
    reset_flags TEXT,
    seq INTEGER,
    radio_stats JSONB,
    mqtt_stats JSONB
);
CREATE INDEX idx_hub_status_hub_time ON hub_status (hub_sn, timestamp DESC);
INSERT INTO hub_status (timestamp, hub_sn, uptime, firmware_revision, rssi, reset_flags, seq, radio_stats, mqtt_stats)
VALUES
  (NOW(), 'HB-001', 100000, '1.0.0', -70, 'none', 1, '{}', '{}'),
  (NOW() - INTERVAL '1 hour', 'HB-001', 99500, '1.0.0', -72, 'none', 2, '{}', '{}');

-- Table: evt_precip
CREATE TABLE evt_precip (
    id SERIAL PRIMARY KEY,
    timestamp TIMESTAMPTZ NOT NULL,
    station_id TEXT NOT NULL,
    hub_sn TEXT NOT NULL
);
CREATE INDEX idx_evt_precip_station_time ON evt_precip (station_id, timestamp DESC);
INSERT INTO evt_precip (timestamp, station_id, hub_sn)
VALUES
  (NOW(), 'ST-00175439', 'HB-001'),
  (NOW() - INTERVAL '1 hour', 'ST-00175439', 'HB-001');

-- Table: evt_strike
CREATE TABLE evt_strike (
    id SERIAL PRIMARY KEY,
    timestamp TIMESTAMPTZ NOT NULL,
    station_id TEXT NOT NULL,
    hub_sn TEXT NOT NULL,
    distance REAL,
    energy INTEGER
);
CREATE INDEX idx_evt_strike_station_time ON evt_strike (station_id, timestamp DESC);
INSERT INTO evt_strike (timestamp, station_id, hub_sn, distance, energy)
VALUES
  (NOW(), 'ST-00175439', 'HB-001', 10.0, 100),
  (NOW() - INTERVAL '1 hour', 'ST-00175439', 'HB-001', 12.0, 120);
