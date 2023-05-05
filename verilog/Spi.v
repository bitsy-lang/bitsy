`timescale 1ns/1ns

module Spi(
    input  wire        clock_12MHz,

    input  wire        read_strobe,
    input  wire [23:0] read_addr,

    output reg  [7:0]  read_data = 0,
    output wire        read_busy,

    output reg         spi_clock = 0,
    output wire        spi_mosi,
    input  wire        spi_miso,
    output wire        spi_ss
);
    localparam IDLE = 0;
    localparam SENDING = 1;
    localparam RECEIVING = 2;
    localparam DONE = 3;

    reg [2:0] state = IDLE;

    reg [31:0] send_data;
    reg [4:0]  i = 0;
    reg [2:0]  j = 0;

    assign spi_mosi = (state == SENDING) ? send_data[i] :  1'b1;
    assign spi_ss = state == IDLE;
    assign read_busy = !spi_ss;

    always @(posedge clock_12MHz) begin
        case (state)
            IDLE: begin
                if (read_strobe) begin
                    send_data <= { 8'h03, read_addr };
                    spi_clock <= 0;
                    i <= 31;
                    j <= 7;
                    state <= SENDING;
                end
            end
            SENDING: begin
                if (spi_clock == 0) begin
                    spi_clock <= 1;
                end else begin
                    spi_clock <= 0;
                    if (i == 0) begin
                        read_data <= 8'b0;
                        state <= RECEIVING;
                    end else begin
                        i <= i - 1;
                    end
                end
            end
            RECEIVING: begin
                if (spi_clock == 0) begin
                    spi_clock <= 1;
                    read_data[j] <= spi_miso;
                end else begin
                    spi_clock <= 0;
                    if (j == 0) begin
                        state <= DONE;
                    end else begin
                        j <= j - 1;
                    end
                end
            end
            DONE: begin
                state <= IDLE;
            end
        endcase
    end
endmodule
