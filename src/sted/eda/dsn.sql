
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
