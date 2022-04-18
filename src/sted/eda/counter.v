(* elk_width=100, elk_height=30 *)
module r(a, b);
   (* elk_port_side="WEST" *) input a;
   (* elk_port_side="EAST" *) output b;
endmodule // r

(* elk_width=30, elk_height=100 *)
module rv(a, b);
   (* elk_port_side="NORTH" *) input a;
   (* elk_port_side="SOUTH" *) output b;
endmodule // rv

module mid(input j, output k);
   rv r1v(j, k);
   r r2(j, k);
endmodule // top

module top();
   wire f, g;
   mid m1(f, g);
   mid m2(f, g);
endmodule   


// module counter (
//   input rst,
//   input clk,
//   output led
    
// );

//     /* reg */
//     reg [7:0] counter = 0;
//     reg state;
    
//     /* assign */
//     assign led = counter[7];
    
//     /* always */
//     always @ (posedge clk) begin
//       if (!rst) begin
//         counter <= 8'b0;
//       end else begin
//         counter <= counter + 1'b1;
//       end
//     end

// endmodule
