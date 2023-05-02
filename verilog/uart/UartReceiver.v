`timescale 1ns/1ns

module UartReceiver(
    input  wire clock_12MHz,
    output reg  [7:0] data = 8'b0,
    output reg  data_valid = 1'b0,
    input  wire ready,
    input  wire uart_rx_wild
);
    wire uart_rx_sync;
    assign uart_rx_sync = uart_rx_wild;

    /*
    UartSynchronizer synchronizer(
        .clock_12MHz(clock_12MHz),
        .uart_rx_wild(uart_rx_wild),
        .uart_rx_sync(uart_rx_sync)
    );
    */

    reg [3:0] state = 0;
    reg [15:0] pulse = 0;

    // 83.33 ns
    always @(posedge clock_12MHz) begin
        case (state)
            // IDLE
            0: begin
                data_valid <= 1'b0;
                data <= 8'b0;

                if (uart_rx_sync == 0) begin
                    pulse <= 1875; // ~= 1250 * 1.5
                    state <= 1;
                end
            end
            // READ BIT 0
            1: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    data[0] <= uart_rx_sync;
                    pulse <= 1250;
                    state <= 2;
                end
            end
            // READ BIT 1
            2: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    data[1] <= uart_rx_sync;
                    pulse <= 1250;
                    state <= 3;
                end
            end
            // READ BIT 2
            3: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    data[2] <= uart_rx_sync;
                    pulse <= 1250;
                    state <= 4;
                end
            end
            // READ BIT 3
            4: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    data[3] <= uart_rx_sync;
                    pulse <= 1250;
                    state <= 5;
                end
            end
            // READ BIT 4
            5: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    data[4] <= uart_rx_sync;
                    pulse <= 1250;
                    state <= 6;
                end
            end
            // READ BIT 5
            6: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    data[5] <= uart_rx_sync;
                    pulse <= 1250;
                    state <= 7;
                end
            end
            // READ BIT 6
            7: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    data[6] <= uart_rx_sync;
                    pulse <= 1250;
                    state <= 8;
                end
            end
            // READ BIT 7
            8: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    data[7] <= uart_rx_sync;
                    pulse <= 1150; // fudge this one a little bit
                    state <= 9;
                end
            end
            // READ STOP BIT
            9: begin
                if (pulse > 0) begin
                    pulse <= pulse - 1;
                end else begin
                    if (uart_rx_sync == 1'b1) begin
                        state <= 10;
                    end else begin
                        state <= 0;
                    end
                end
            end
            // WAIT FOR READY
            10: begin
                data_valid <= 1'b1;
                if (ready) begin
                    state <= 0;
                end
            end
        endcase
    end
endmodule

module UartSynchronizer(
    input  wire clock_12MHz,
    input wire uart_rx_wild,
    output reg uart_rx_sync
);
    reg buffer1;

    always @(posedge clock_12MHz) begin
        buffer1 <= uart_rx_wild;
        uart_rx_sync <= buffer1;
    end
endmodule
