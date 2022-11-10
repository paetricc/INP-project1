-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2021 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Tomas Bartu (xbartu11)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- ram[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_WREN  : out std_logic;                    -- cteni z pameti (DATA_WREN='0') / zapis do pameti (DATA_WREN='1')
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA obsahuje stisknuty znak klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna pokud IN_VLD='1'
   IN_REQ    : out std_logic;                     -- pozadavek na vstup dat z klavesnice
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- pokud OUT_BUSY='1', LCD je zaneprazdnen, nelze zapisovat,  OUT_WREN musi byt '0'
   OUT_WREN : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

----- PC -----
	signal pc_reg : std_logic_vector(11 downto 0);
	signal pc_inc : std_logic;
	signal pc_dec : std_logic;
----- PC -----

----- CNT -----
	signal cnt_reg : std_logic_vector(11 downto 0);
	signal cnt_inc : std_logic;
	signal cnt_dec : std_logic;
----- CNT -----

----- PTR -----
	signal ptr_reg : std_logic_vector(9 downto 0);
	signal ptr_inc : std_logic;
	signal ptr_dec : std_logic;
----- PTR -----

----- FSM STATES -----
	type fsm_state is(
		s_start, s_fetch, s_decode,
	
		--- '>' and '<' STATES ---
		s_ptr_inc, s_ptr_dec,
		--- '>' and '<' STATES ---
		
		--- '+' STATES ---
		s_value_inc_read, s_value_inc_write,
		--- '+' STATES ---
		
		--- '-' STATES ---
		s_value_dec_read, s_value_dec_write,
		--- '-' STATES ---
		
		--- '[' STATES ---
		s_while_start_read, s_while_start_data, 
		s_while_start_end, s_while_start_code,
		--- '[' STATES ---
		
		--- ']' STATES ---
		s_while_end_read, s_while_end_data, s_while_end_end,
		s_while_end_cond, s_while_end_code,
		--- ']' STATES ---
	
		--- '.' STATES ---
		s_print_enable, s_print_data,
		--- '.' STATES ---
		
		--- ',' STATES ---
		s_store_enable, s_store_data,
		--- ',' STATES ---
		
		--- '~' STATES ---
		s_while_break_read, s_while_break_end, s_while_break_code,
		--- '~' STATES ---
		
		--- "NULL" STATE --- 
		s_null,
		--- "NULL" STATE ---
		
		--- NONDEF STATE ---
		s_others
		--- NONDEF STATE ---
		);
	signal pstate : fsm_state;
	signal nstate : fsm_state;	
----- FSM STATES -----

----- DECODER STATES -----
type dec_state is (
	i_ptr_inc,
	i_ptr_dec,
	i_value_inc,
	i_value_dec,
	i_while_start,
	i_while_end,
	i_while_break,
	i_print,
	i_store,
	i_null,
	i_nop
);
signal code : dec_state;
----- DECODER STATES -----

----- MX -----
		signal mx_select : std_logic_vector(1 downto 0);
----- MX -----

begin

----- PC -----	
	pc: process (RESET, CLK) is
	begin
		if (RESET = '1') then
			pc_reg <= (others=>'0');
		elsif rising_edge(CLK) then
			if (pc_inc = '1') then
				pc_reg <= pc_reg + 1;
			elsif (pc_dec = '1') then
				pc_reg <= pc_reg - 1;
			end if;
		end if;
	end process pc;
----- PC -----

----- CNT -----
	cnt: process (RESET, CLK) is
	begin
		if (RESET = '1') then
			cnt_reg <= (others=>'0');
		elsif rising_edge(CLK) then
			if (cnt_inc = '1') then
				cnt_reg <= cnt_reg + 1;
			elsif (cnt_dec = '1') then
				cnt_reg <= cnt_reg - 1;
			end if;
		end if;
	end process cnt;
----- CNT -----

----- PTR -----
	ptr: process (RESET, CLK) is
	begin
		if RESET = '1' then
			ptr_reg <= (others=>'0');
		elsif rising_edge(CLK) then
			if (ptr_inc = '1') then
				ptr_reg <= ptr_reg + 1;
			elsif (ptr_dec = '1') then
				ptr_reg <= ptr_reg - 1;
			end if;
		end if;
	end process ptr;
----- PTR -----	
	
----- MX -----
	mx: process (mx_select) is
	begin
			case mx_select is
				when "00"   => DATA_WDATA <= IN_DATA;
				when "01"   => DATA_WDATA <= DATA_RDATA + 1;
				when "10"   => DATA_WDATA <= DATA_RDATA - 1;
				when "11"   => DATA_WDATA <= "00000000";
				when others => DATA_WDATA <= "00000000";
				end case;
	end process mx;
----- MX -----

	CODE_ADDR <= pc_reg;
	DATA_ADDR <= ptr_reg;
	OUT_DATA  <= DATA_RDATA;

----- DEC -----
decoder: process(CODE_DATA) is begin
	case (CODE_DATA) is
		when x"3E"  => code <= i_ptr_inc;
		when x"3C"  => code <= i_ptr_dec;
		when x"2B"  => code <= i_value_inc;
		when x"2D"  => code <= i_value_dec;
		when x"5B"  => code <= i_while_start;
		when x"5D"  => code <= i_while_end;
		when x"2E"  => code <= i_print;
		when x"2C"  => code <= i_store;
		when x"7E"  => code <= i_while_break;
		when x"00"  => code <= i_null;
		when others => code <= i_nop;
	end case;
end process decoder;
----- DEC -----

----- FSM pstate -----
	pstate_logic: process (CLK, RESET, EN) is
	begin
		if (RESET = '1') then
			pstate <= s_start;
		elsif rising_edge(CLK) then
			if (EN = '1') then
				pstate <= nstate;
			end if;
		end if;
	end process pstate_logic;
----- FSM pstate -----
	
----- FSM nstate -----
	nstate_logic: process (pstate, code, DATA_RDATA, cnt_reg, CODE_DATA, OUT_BUSY, IN_VLD) is begin
		nstate    <= s_start;
		OUT_WREN  <= '0';
		IN_REQ    <= '0';
		CODE_EN   <= '0';
		DATA_WREN <= '0';
		DATA_EN   <= '0';
		pc_inc    <= '0';
		pc_dec    <= '0';
		cnt_inc   <= '0';
		cnt_dec   <= '0';
		ptr_inc   <= '0';
		ptr_dec   <= '0';
		mx_select <= "00";
		
		case pstate is
			--- vychozi stav
			when s_start => nstate <= s_fetch;
			--- nacti instrukci
			when s_fetch =>
				CODE_EN <= '1';
				nstate  <= s_decode;
			--- dekoduj instrukci
			when s_decode =>
				case code is
					when i_ptr_inc     => nstate <= s_ptr_inc;
					when i_ptr_dec     => nstate <= s_ptr_dec;
					when i_value_inc   => nstate <= s_value_inc_read;
					when i_value_dec   => nstate <= s_value_dec_read;
					when i_while_start => nstate <= s_while_start_read;
					when i_while_end   => nstate <= s_while_end_read;
					when i_print       => nstate <= s_print_enable;
					when i_store       => nstate <= s_store_enable;
					when i_while_break => nstate <= s_while_break_read;
					when i_null        => nstate <= s_null;
					when others        => nstate <= s_others;
				end case;
				
				----- inkrementace hodnoty ukazatele (>) ----------
				when s_ptr_inc =>
					ptr_inc <= '1';
					pc_inc  <= '1';
					nstate  <= s_fetch;
				---------------------------------------------------

				----- dekrementace hodnoty ukazatele (<) ----------
				when s_ptr_dec =>
					ptr_dec <= '1';
					pc_inc  <= '1';
					nstate  <= s_fetch;
				---------------------------------------------------
				
				----- inkrementace hodnoty aktualni bunky (+) -----
				--- nactu data
				--- DATA_RDATA ‹ ram[PTR]
				when s_value_inc_read =>
					DATA_EN   <= '1';
					DATA_WREN <= '0';
					nstate    <= s_value_inc_write;
					
				--- ram[PTR] ‹ DATA_RDATA
				when s_value_inc_write =>
					mx_select <= "01";
					DATA_EN   <= '1';
					DATA_WREN <= '1';
					pc_inc    <= '1';
					nstate    <= s_fetch;
				---------------------------------------------------
				
				----- dekrementace hodnoty aktualni bunky (-) -----
				--- nactu data
				--- DATA_RDATA  ram[PTR]
				when s_value_dec_read =>
					DATA_WREN <= '0';
					DATA_EN   <= '1';
					nstate    <= s_value_dec_write;
					
				--- ram[PTR] ‹ DATA_RDATA
				when s_value_dec_write =>
					mx_select <= "10";
					DATA_WREN <= '1';
					DATA_EN   <= '1';
					pc_inc    <= '1';
					nstate    <= s_fetch;
				---------------------------------------------------
				
				------------- zacatek while cyklu ([) -------------
				--- DATA_RDATA < ram[PTR]
				when s_while_start_read =>
					DATA_WREN <= '0';
					DATA_EN   <= '1';
					pc_inc    <= '1';
					nstate    <= s_while_start_data;

				when s_while_start_data =>
					--- if (ram[PTR] == 0)
					if (DATA_RDATA = "00000000") then
						cnt_inc <= '1';
						nstate  <= s_while_start_code;
					else
						nstate  <= s_fetch;
					end if;

				--- c < rom[PC]
				when s_while_start_code =>
					CODE_EN <= '1';
					nstate  <= s_while_start_end;
					
				--- while (CNT != 0)
				when s_while_start_end =>
					if (cnt_reg /= "000000000000") then
						--- if (c == "[")
						if (CODE_DATA = x"5B") then
							cnt_inc <= '1';
						--- elsif (c == "]")
						elsif (CODE_DATA = x"5D") then
							cnt_dec <= '1';
						end if;
						pc_inc <= '1';
						nstate <= s_while_start_code;
					else
						nstate <= s_fetch;
					end if;
				---------------------------------------------------
				
				-------------- konec while cyklu (]) --------------
				--- DATA_RDATA < ram[PTR]
				when s_while_end_read =>
					DATA_WREN <= '0';
					DATA_EN   <= '1';
					nstate	 <= s_while_end_data;
					
				when s_while_end_data =>
					--- if (ram[PTR] == 00)
					if (DATA_RDATA = "00000000") then
						pc_inc  <= '1';
						nstate  <= s_fetch;
					else
						cnt_inc <= '1';
						pc_dec  <= '1';
						nstate  <= s_while_end_code;
					end if;

				--- c < rom[PC]
				when s_while_end_code =>
					CODE_EN <= '1';
					nstate  <= s_while_end_end;
					
				--- while (CNT != 0)	
				when s_while_end_end =>
					if (cnt_reg /= "000000000000") then
						--- if (c == "[")
						if (CODE_DATA = x"5D") then
							cnt_inc <= '1';
						--- elsif (c == "]")
						elsif (CODE_DATA = x"5B") then
							cnt_dec <= '1';
						end if;
						nstate <= s_while_end_cond;
					else
						nstate <= s_fetch;
					end if;
					
				when s_while_end_cond =>
					--- if (CNT == 0)
					if (cnt_reg = "000000000000") then
						pc_inc <= '1';
					--- else
					else
						pc_dec <= '1';
					end if;
					nstate <= s_while_end_code;	
				---------------------------------------------------
				
				------- vytiskni hodnotu aktualni bunky (.) -------
				--- povolime nacteni dat z RAM
				when s_print_enable =>
					DATA_WREN    <= '0';
					DATA_EN      <= '1';
					nstate       <= s_print_data;
					
				when s_print_data =>
					--- while (OUT_BUSY) {}
					if (OUT_BUSY = '1') then
						DATA_WREN <= '0';
						DATA_EN   <= '1';
						nstate    <= s_print_data;
					--- OUT_DATA ‹ ram[PTR]
					else
						OUT_WREN  <= '1';
						pc_inc    <= '1';
						nstate    <= s_fetch;
					end if;
				---------------------------------------------------
				
				-- nacti hodnotu a uloz ji do aktualni bunky (,) --
				when s_store_enable =>
				--- chceme si vzit data
					IN_REQ       <= '1';
					mx_select    <= "00";
					nstate       <= s_store_data;
					
				when s_store_data =>
					--- cekame na validni data
					--- while (!IN_VLD) {}
					if (IN_VLD /= '1') then
						IN_REQ    <= '1';
						nstate    <= s_store_data;
					--- ram[PTR] ‹ IN_DATA
					else
						mx_select <= "00";
						IN_REQ    <= '0';
						DATA_WREN <= '1';
						DATA_EN   <= '1';
						pc_inc    <= '1';
						nstate    <= s_fetch;
					end if;
				---------------------------------------------------
				
				----- ukonci prave provadenou smycku while (~) ----
				when s_while_break_read =>
					pc_inc  <= '1';
					cnt_inc <= '1';
					nstate  <= s_while_break_code;
					
				--- c < rom[PC]
				when s_while_break_code =>
					CODE_EN <= '1';
					nstate  <= s_while_break_end;	
					
				when s_while_break_end =>
					--- if (CNT )
					if (cnt_reg /= "000000000000") then
						--- if (c == "[")
						if (CODE_DATA = x"5B") then
							cnt_inc <= '1';
						--- elsif (c == "]")
						elsif (CODE_DATA = x"5D") then
							cnt_dec <= '1';
						end if;
						pc_inc <= '1';
						nstate <= s_while_start_code;
					else
						nstate <= s_fetch;
					end if;
				---------------------------------------------------
				
				-------- zastav vykonavani programu (null) --------
				when s_null => nstate <= s_null;
				---------------------------------------------------
				
				------------ nelze zpracovat instrukci ------------
				--- preskocim instrukci
				--- PC ‹ PC + 1
				when s_others =>
					pc_inc <= '1';
					nstate <= s_fetch;
				---------------------------------------------------
				
				when others => null;
		end case;
	end process nstate_logic;
----- FSM nstate -----

end behavioral;