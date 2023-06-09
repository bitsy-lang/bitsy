`timescale 1ns/1ns

module UartSender(
    input  wire clock_12MHz,
    input  wire [7:0] data,
    input  wire data_valid,
    output wire ready,
    output reg uart_tx = 1'b1
);

    reg [7:0] latched_data;
    reg [3:0] state = 0;
    reg [10:0] pulse = 0;

    assign ready = state == 0;

    always @(posedge clock_12MHz) begin
        case (state)
            // IDLE
            0: begin
                if (data_valid) begin
                    latched_data <= data;
                    uart_tx <= 1'b0;
                    pulse <= 1250;
                    state <= 1;
                end
            end
            // SEND BIT 0
            1: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    uart_tx <= latched_data[0];
                    pulse <= 1250;
                    state <= 2;
                end
            end
            // SEND BIT 1
            2: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    uart_tx <= latched_data[1];
                    pulse <= 1250;
                    state <= 3;
                end
            end
            // SEND BIT 2
            3: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    uart_tx <= latched_data[2];
                    pulse <= 1250;
                    state <= 4;
                end
            end
            // SEND BIT 3
            4: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    uart_tx <= latched_data[3];
                    pulse <= 1250;
                    state <= 5;
                end
            end
            // SEND BIT 4
            5: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    uart_tx <= latched_data[4];
                    pulse <= 1250;
                    state <= 6;
                end
            end
            // SEND BIT 5
            6: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    uart_tx <= latched_data[5];
                    pulse <= 1250;
                    state <= 7;
                end
            end
            // SEND BIT 6
            7: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    uart_tx <= latched_data[6];
                    pulse <= 1250;
                    state <= 8;
                end
            end
            // SEND BIT 7
            8: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    uart_tx <= latched_data[7];
                    pulse <= 1250;
                    state <= 9;
                end
            end
            // SEND STOP BIT
            9: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    uart_tx <= 1;
                    pulse <= 1250;
                    state <= 10;
                end
            end
            // YEAH
            10: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    state <= 0;
                end
            end
        endcase
    end
endmodule
