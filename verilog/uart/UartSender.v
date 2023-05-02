`timescale 1ns/1ns

// 9600 Baud
module UartSender(
    input  wire clock_12MHz,
    input  wire [7:0] data,
    input  wire data_valid,
    output reg ready = 1'b1,
    output reg uart_tx = 1'b1
);
    localparam TICKS_PER_BIT = 1250; // 9600 baud running at 12MHz: (12 MHz) / (9600 Hz) = 1250

    reg [7:0] latched_data;
    reg [3:0] state = 0;
    reg [10:0] pulse = 0;

    always @(posedge clock_12MHz) begin
        if (pulse == 0) begin
            case (state)
                0: begin
                    if (data_valid) begin
                        latched_data <= data;
                        state <= 1;
                        uart_tx <= 1'b0;
                        ready <= 1'b0;
                    end
                end
                1: begin
                    uart_tx <= latched_data[0];
                    state <= 2;
                end
                2: begin
                    uart_tx <= latched_data[1];
                    state <= 3;
                end
                3: begin
                    uart_tx <= latched_data[2];
                    state <= 4;
                end
                4: begin
                    uart_tx <= latched_data[3];
                    state <= 5;
                end
                5: begin
                    uart_tx <= latched_data[4];
                    state <= 6;
                end
                6: begin
                    uart_tx <= latched_data[5];
                    state <= 7;
                end
                7: begin
                    uart_tx <= latched_data[6];
                    state <= 8;
                end
                8: begin
                    uart_tx <= latched_data[7];
                    state <= 9;
                end
                9: begin
                    uart_tx <= 1;
                    state <= 10;
                end
                default: begin
                    ready <= 1'b1;
                    state <= 0;
                end
            endcase
            pulse <= TICKS_PER_BIT;
        end else begin
            pulse <= pulse - 1;
        end
    end
endmodule
