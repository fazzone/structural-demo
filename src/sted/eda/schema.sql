select InitSpatialMetaDataFull('NONE');

insert into spatial_ref_sys(srid,auth_srid,auth_name,ref_sys_name,proj4text,srtext)
values
 (0x6d6d, 0x6d6d, 'si', 'millimeter relative', 'not implemented', 'not implemented'),
 (0x756d, 0x756d, 'si', 'micrometer relative', 'not implemented', 'not implemented');

-- Store kicad modules and footprint drawings "verbatim", except for:
-- added _mm units to columns
-- fp_x drawing commands translated to SVG d format (d_mm)
create table kicad_footprint(
       id integer primary key
       , name string
       , layer string
       , tedit string
       , descr string
       , tags_json string
       , attr string
       , version string
       , generator string
);

create table kicad_footprint_draw(
       id integer primary key
       , kicad_footprint_id integer not null
       , layer string
       , d_mm string 
       , width_mm real -- also represents font thickness  
       , text_type string
       , text string
       , font_size_w_mm real
       , font_size_h_mm real
       , wkt string
);

create table kicad_pad(
       id integer primary key
       , type string
       , shape string
       , width_mm real
       , height_mm real
       , d_mm string
       , wkt string
       , refcount integer default 1
);

create unique index kicad_pad_unique on kicad_pad(
       type, shape, width_mm, height_mm, d_mm
);

create table kicad_footprint_pad(
       id integer primary key
       , kicad_footprint_id integer not null
       , kicad_pad_id integer not null
       , name string
       , x_mm real
       , y_mm real
       , drill_mm real
       , layers_json string
);

create table specctra_dsn(
       id integer primary key
       , pcb_id string
       , host_cad string
       , host_version string
       , resolution_unit string
       , resolution real
       , unit string

       , pcb_boundary_shape blob
);

create table dsn_layer(
       id integer primary key
       , dsn_id integer
       , type string
       , idx integer
);

create table dsn_plane(
       id integer primary key
       , dsn_id integer
       , name string
       , aperture_width real
       , dsn_layer_id integer
       , plane_shape blob
);

create table specctra_window(
       id integer primary key
       , dsn_id integer
       , dsn_plane_id integer
       , window_shape blob
);
