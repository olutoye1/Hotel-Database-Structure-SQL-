
---------------------------------------------------------------------------------------
--MEMBER 1 PROCEDURES--
--Name:<Dennis Therine Sanoy>

--<AddNewHotel>--

Create or replace Procedure AddNewHotel (
    hotel_name              in  varchar2, -- input param to identify the hotel by it’s name.
    street                  in  varchar2, -- input param to identify the street the hotel is on.
    city                    in  varchar2, -- input param to identify the city the hotel is in.
    hotel_state             in  varchar2, -- input param to identify the state the hotel is in.
    hotel_phone             in  number)   -- input param to add the hotel’s phone number.
    as 
    
    count_rows                  number;   -- counts the quantity of rows in the hotel table.
    next_id                     number;   -- the number of the id for the new hotel that is going to added into the table.
    hotel_status                varchar2(50) := 'Owned'; --the status of the hotel is now "owned", by default. This is so it can be "sold" later on.
    
    Duplicate_phone_Exception   exception;--a user defined exception that is raised
    errorCode                   integer := 0;
    
    h_phonenum                  number;
Begin
    select count(rownum) into count_rows from hotel; --find number of rows in the hotel table.
    for i in 1..count_rows 
    loop                                             --loop through the hotel table to check if there is an identical phone number that already exists in the table.
        select phone into h_phonenum
        from hotel where hotelBranchID = i;
        if (h_phonenum = hotel_phone) then
            raise Duplicate_phone_Exception;         --raise exception for duplicate phone number.
            errorCode := -1;
            exit;
        end if;
    end loop;
    
    if (errorCode != 1) then                         --if the exception is not raised, then proceed to insert the new hotel's info into the table.
        next_id := count_rows +1;
        Insert Into hotel values (next_id, city, street, hotel_state, hotel_phone, hotel_status, hotel_name);
    end if;
Exception                                            --handle exception, print out message that notifies the user of the duplicate phone number.
    when Duplicate_phone_Exception then
    dbms_output.put_line('The phone number you entered already exists in the database.');
End AddNewHotel;

--<FindHotel>--

Create or replace Procedure FindHotel (
    city                    in  varchar2, -- input param to identify the city the hotel is in.
    street                  in  varchar2, -- input param to identify the street the hotel is on.
    hotel_state             in  varchar2) -- input param to identify the state the hotel is in.
    as
    hotel_id                number; -- output that will be returned for hotels with a matching address as the user input.
    errorCode               number := -1; -- output returns 0 if a match was found, negative number if no match was found.
    
    ch_city                 varchar2(50);
    ch_street               varchar2(50);
    ch_state                varchar2(50);
    
    No_match_found          exception;
    counter                 number;
Begin
    select count(rownum) into counter from hotel; --find number of rows in the hotel table.
    for i in 1..counter loop                     --loop for as many rows in the hotel table to check each row's values one by one.
        select city into ch_city from hotel where hotelBranchID = i;
        if (ch_city = city) then                                            --check if the city in the current row of table the loop is on is identical to the city parameter.
            select street into ch_street from hotel where hotelBranchID = i;
            if ch_street = street then                                      --same as the previous if statement, but checking street.
                select state_ into ch_state from hotel where hotelBranchID = i;
                if (ch_state = hotel_state) then                            --same as the previous if statement, but checking state.
                    hotel_id := i;                                          --once all checks have been confirmed, the address matches and the current i that
                    errorCode := 0;                                         --the loop is on is the hotel we are looking for. there is no error, so errorCode is set to 0.
                    exit;                                                   --leave the loop.
                end if;
            end if;        
        end if;
    end loop;   
    if (errorCode != -1) then                       --if the errorCode was changed so it is no longer an error, msg output the hotel id that was found.
        dbms_output.put_line('A Hotel with a matching address was found in the database. Hotel ID: '|| hotel_id);
    else
        raise No_match_found;                      --exception raised if no match was found, since the errorCode was not changed.
    end if;
Exception
    when No_match_found then                       --exception prints out a message that no match was found.
    dbms_output.put_line('No hotel found with matching address.');
End FindHotel;

--<AddRoomtoHotel>--

Create or replace Procedure AddRoomtoHotel (
    hotel_id            IN number,   -- input param to identify the hotel ID of the hotel that will be given more rooms.
    roomType            IN varchar2,   -- input param to identify what type of room will be added to the hotel. -- Single, Double, Suite, Conference
    room_Quantity       IN number)   -- input param to determine the quantity of rooms of the chosen type will be added to the hotel.
    as
    
    --count_rows                  number;   -- counts the quantity of rows in the rooms table.
    count_id_found              number;   -- count number of id the same as hotel_id found in the table.
    next_id                     number;   -- the number of the id for the new room that is going to added into the table.
    counter                     number;   --used for the for loop as the end quantifier.
    
    errorCode                   integer := 0; -- return a negative number for errors. default 0.
    Hotel_id_not_Found          exception;    -- exception for when hotel_id requested does not exist in the database.
Begin
    select count(rownum) into counter from rooms;
    
    select count(hotelBranchID) into count_id_found from hotel where hotelBranchID = hotel_id;
    if (count_id_found = 0) then
        raise Hotel_id_not_Found;
        errorCode := -1;
    else   
        for i in 1..room_Quantity loop
            next_id := counter + i;
            insert into rooms values(next_id, roomType, hotel_id);
            --dbms_output.put_line('Room ID: '||next_id||', Room Type: '||roomType||', Quantity: '||i);          
        end loop;
        dbms_output.put_line('Room[s] added successfully');
    end if;

Exception
    when Hotel_id_not_Found then
    dbms_output.put_line('No hotel found with matching ID: '||hotel_id);
End AddRoomtoHotel;

--<SellExistingHotel>--

Create or replace Procedure SellExistingHotel (
    hotel_ID   IN number)      -- input param to identify the specific hotel being sold.
    as
    
    cursor hotel_cursor is
    select city, street, state_, phone, hotelStatus, hotelName
    from hotel
    where hotelBranchID = hotel_ID;
    
    h_row hotel_cursor%rowtype;
    
    errorCheck           number;        -- variable used to check for number of rows returned from implicit cursor
    Hotel_id_not_Found   exception;     -- exception for when hotel_id requested does not exist in the database.
Begin
    
    select count(hotelBranchID) into errorCheck
    from hotel
    where hotelBranchID = hotel_ID;
    
    if (errorCheck != 1) then
        raise hotel_id_not_found;
    else
        open hotel_cursor;
        fetch hotel_cursor into h_row;
        dbms_output.put_line('Hotel being sold: ' || h_row.hotelName);
        dbms_output.put_line('Location: ' || h_row.street || ', ' || h_row.city || ', ' || h_row.state_);
        dbms_output.put_line('Phone number: ' || h_row.phone);
        
        update hotel set hotelStatus = 'Sold'
        where hotelBranchID = hotel_ID;
        
        dbms_output.put_line('Hotel status is: ' || h_row.hotelStatus);
        close hotel_cursor;
    end if;
Exception
    when Hotel_id_not_Found then
        dbms_output.put_line('The Hotel ID requested was not found in the directory.');
End SellExistingHotel;

--<ReportHotelsInState>--

Create or replace Procedure ReportHotelsInState (
    in_state IN varchar2) -- input param to determine chosen state to look for hotels in.
    as
    
    cursor hotel_cursor is
    select hotelName, street, city, phone
    from hotel
    where state_ = in_state;
    
    h_row hotel_cursor%rowtype;
    
    errorCheck          integer;
    Hotel_not_found     exception;
Begin
    select count(*) into errorCheck from hotel
    where state_ = in_state;

    if (errorCheck > 0) then
        open hotel_cursor;
        loop
            fetch hotel_cursor into h_row;
            exit when hotel_cursor%notfound;
            dbms_output.put_line('Hotels found in state: '|| in_state);
            dbms_output.put_line('');
            dbms_output.put_line('Name:     '|| h_row.hotelName);
            dbms_output.put_line('Location: '|| h_row.street ||', '|| h_row.city);
            dbms_output.put_line('Phone:    '|| h_row.phone);
            dbms_output.put_line('');
        end loop;
        close hotel_cursor;
    else
        raise Hotel_not_found;
    end if;
Exception
    when Hotel_not_found then
        dbms_output.put_line('No hotels found in '||in_state||' state.');
End ReportHotelsInState;

--END OF MEMBER 1 PROCEDURES--
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
--MEMBER 2 PROCEDURES--
--Name:<Jason Leonard>

--<MakeReservation>--

EXEC MakeReservation('Rida','Atkinson',1,'Single',TO_DATE('2020-11-15','YYYY-MM-DD'),TO_DATE('2020-11-20','YYYY-MM-DD'),TO_DATE('2020-11-11','YYYY-MM-DD'));

CREATE OR REPLACE PROCEDURE MakeReservation (fName IN VARCHAR, lName IN VARCHAR, hotelInput IN INTEGER, roomRequest IN VARCHAR, checkInDate IN DATE, checkOutDate IN DATE, reservationDate IN DATE)
IS

vacantRoom INTEGER := 0; -- ROOM NUMBER
requestedHotel INTEGER := 0; -- HOTEL OF ROOM
generatedRoom INTEGER := 0; -- ROOM ID
requestCustomer INTEGER := 0; -- CUSTOMER ID

invalid_hotel_id EXCEPTION; -- INVALID HOTEL
invalid_date_range EXCEPTION; -- INVALID DATE ORDER
invalid_room_type EXCEPTION; -- INVALID ROOM TYPE

--Stores all rooms with the matching room type
CURSOR findRooms
IS
    SELECT *
    FROM p_reservations
    RIGHT JOIN p_rooms
    ON p_reservations.roomid = p_rooms.roomid
    WHERE p_rooms.roomtype = roomRequest
    AND p_rooms.hotelBranchID = hotelInput;
    
    /*  -- DATA STORED IN CURSOR --
        SELECT *
        FROM p_reservations
        RIGHT JOIN p_rooms
        ON p_reservations.roomid = p_rooms.roomid
        WHERE p_rooms.roomtype = 'Single'
        AND p_rooms.hotelBranchID = 1;
    */

roomRow findRooms%rowtype;

BEGIN

    IF hotelInput <= 0 THEN --If the user inputs a value for Hotel Branch ID that is technially impossible, raise an exception
        RAISE invalid_hotel_id;
    END IF;
    
    IF checkIndate > checkOutDate THEN --User inputs dates in the the wrong order
        RAISE invalid_date_range;
    END IF;
    
    IF roomRequest != 'Single' OR roomRequest != 'Double' OR roomRequest != 'Suite' OR roomRequest != 'Conference' THEN -- User requests a room type that is not possible
        RAISE invalid_room_type;
    END IF;

    --Find a vacant room w/ matching room type in a specfic Hotel
    FOR roomRow IN findRooms
        LOOP
            IF roomRow.resID IS NULL THEN
                dbms_output.put_line(roomRow.roomNum);
                vacantRoom := roomRow.roomNum;
                dbms_output.put_line(roomRow.hotelBranchID);
                requestedHotel := roomRow.hotelBranchID;
                EXIT;
            END IF;
        END LOOP;
    
    --Find the vacant room's ID
    SELECT roomID INTO generatedRoom
    FROM p_rooms
    WHERE hotelbranchid = requestedHotel AND roomnum = vacantRoom;
    
    dbms_output.put_line(generatedRoom);

    --Find the pre existing customer in the customer table
    SELECT customerID INTO requestCustomer
    FROM p_customers
    WHERE firstName = fName AND lastName = lName;
    
    dbms_output.put_line(requestCustomer);
    
    INSERT INTO p_reservations
    VALUES (resid.nextval, reservationDate, checkInDate, checkOutDate, requestCustomer, generatedRoom, 0);    
    
    EXCEPTION
        WHEN invalid_hotel_id THEN
            DBMS_OUTPUT.PUT_LINE('Hotel ID must be greater than 0');
        WHEN invalid_date_range THEN
            DBMS_OUTPUT.PUT_LINE('The dates for the reservation are impossible');
        WHEN invalid_room_type THEN
            DBMS_OUTPUT.PUT_LINE(roomRequest || ' is an invalid room type.');
        WHEN others THEN
             DBMS_OUTPUT.PUT_LINE('The system encountered an error.');   
    
END;

--<FindReservation>--

EXEC FindReservation('Magnus','Acosta', TO_DATE('2020-11-11','YYYY-MM-DD'), 2);

CREATE OR REPLACE PROCEDURE FindReservation (fName IN VARCHAR, lName IN VARCHAR, reservationDate IN DATE, hotelInput IN INTEGER)
IS

requestCustomer INTEGER := 0;
requestRes INTEGER := 0;

invalid_hotel_id EXCEPTION; -- INVALID HOTEL
customer_not_found EXCEPTION; -- Could not find customer
res_not_found EXCEPTION; -- Could not find a matching reservation

BEGIN

    IF hotelInput <= 0 THEN --If the user inputs a value for Hotel Branch ID that is technially impossible, raise an exception
        RAISE invalid_hotel_id;
    END IF;
    
    SELECT customerID INTO requestCustomer
    FROM p_customers
    WHERE firstName = fName AND lastName = lName;
    
    SELECT resid INTO requestRes
    FROM p_reservations
    LEFT JOIN p_rooms
    ON p_reservations.roomid = p_rooms.roomid
    WHERE p_reservations.customerid = requestCustomer
    AND p_reservations.resdate = reservationDate
    AND p_rooms.hotelBranchId = hotelInput;

    /*
    -- DATA RETURNED FROM SELECT --
    SELECT resid
    FROM p_reservations
    LEFT JOIN p_rooms
    ON p_reservations.roomid = p_rooms.roomid
    WHERE p_reservations.customerid = 1;
    */
    
    IF requestCustomer = 0 THEN
        RAISE customer_not_found;
    END IF;
    
    IF requestRes = 0 THEN
        RAISE res_not_found;
    END IF;
    
    DBMS_OUTPUT.PUT_LINE('Customer Reservation ID: ' || requestRes);

    EXCEPTION
        WHEN customer_not_found THEN
            DBMS_OUTPUT.PUT_LINE('The Customer could not be found'); 
        WHEN res_not_found THEN
            DBMS_OUTPUT.PUT_LINE('a Reservation w/ that criteria for that Customer could not be found');
        WHEN others THEN
            DBMS_OUTPUT.PUT_LINE('The System encountered an problem.');
END;

--<CancelReservation>--

EXEC CancelReservation (10);

CREATE OR REPLACE PROCEDURE CancelReservation (reservationID IN VARCHAR)
IS

already_cancelled EXCEPTION; -- exception when the reservation is already cancelled

requestCustomer INTEGER := 0; -- holds the value for the cancelled reservation. 1 = cancelled ,0 = active

BEGIN
    --Find the reservation value for the requested reservation
    SELECT rescancel INTO requestCustomer
    FROM p_reservations
    WHERE resid = reservationID;
    
    IF requestCustomer = 1 THEN 
        RAISE already_cancelled;
    
    ELSE
        UPDATE p_reservations
        SET rescancel = 1
        WHERE resid = reservationID;
        DBMS_OUTPUT.PUT_LINE('Reservation '|| reservationID || ' cancelled.');
    END IF;
    
    EXCEPTION
        WHEN already_cancelled THEN
             DBMS_OUTPUT.PUT_LINE('Reservation is already cancelled.');
        WHEN no_data_found THEN
             DBMS_OUTPUT.PUT_LINE('No Reservation ID Found');
        WHEN others THEN
             DBMS_OUTPUT.PUT_LINE('The System has encountered an ERROR');
END;

--<ShowCancelations>--

EXEC ShowCancelations; 

CREATE OR REPLACE PROCEDURE ShowCancelations
IS

CURSOR cancelCursor
IS
SELECT *
FROM p_reservations
WHERE resCancel = 1;

resRow cancelCursor%rowtype;

BEGIN
    DBMS_OUTPUT.PUT_LINE('Cancelled Reservations');
    DBMS_OUTPUT.PUT_LINE('----------------------');
    FOR resRow IN cancelCursor
        LOOP
            DBMS_OUTPUT.PUT_LINE('Reservation ID: ' || resRow.resid);
            DBMS_OUTPUT.PUT_LINE('Customer ID: ' || resRow.customerid);
            DBMS_OUTPUT.PUT_LINE('');
        END LOOP;
END;

--END OF MEMBER 2 PROCEDURES--
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
--MEMBER 3 PROCEDURES--
--Name:<TYPE NAME HERE>

--<INSERT PROCEDURES HERE>--


--END OF MEMBER 3 PROCEDURES--
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
--MEMBER 4 PROCEDURES--
--Name:Olutoye Sekiteri

--<INSERT PROCEDURES HERE>--


--15)
CREATE OR REPLACE PROCEDURE AddServiceToReservation (inputServID in number, inputServ in varchar, resID in number, servDate in Date) AS

BEGIN
        -- if service input is one of the available services move forward else invalid msg
            IF (inputServ = 'Laundry' Or inputServ = 'Room Service') THEN 
        -- insert all values to service table
                INSERT INTO Services VALUES (inputServID, inputServ, TO_DATE(servDate,'DD-MON-RR'),resID);
        ELSE
            dbms_output.put_line('Invalid Service Input');
            -- Invalid Message Output
        END IF;
END;

--16)
CREATE OR REPLACE PROCEDURE ListReservationService (resID in number )IS CURSOR listServ_cursor IS
SELECT services_id, typeofservice, daterequested, services.res_id
FROM services
WHERE services.res_id = resID;
-- Create cursor with all service elements required

s_row listServ_cursor%rowtype;
-- Assign cursor to variable
counter321 number;
--counter to check reservations existance
BEGIN
counter321 := 0; 
-- assign default value of 0 to counter
FOR s_row IN listServ_cursor
LOOP

Dbms_output.put_line('Service ID:'|| s_row.services_id||'  Types of Service:'|| 
s_row.typeofservice||'  Date Requested:  '||s_row.daterequested||'  Reservation ID:  ' ||s_row.res_id);
-- Iterate over every row of cursor and output information

counter321 := counter321 + 1;
--add one to counter for every row in cursor
END LOOP;

IF ( counter321 = 0 ) THEN
    Dbms_output.put_line('There are no services for that Reservation ID');
END IF;
--if counter is still at default value then for loop was never engaged, meaning the input doesnt exist

END;



-- 17)
CREATE OR REPLACE PROCEDURE ShowServiceReport (userInput1 in varchar)IS CURSOR showServ_cursor IS
SELECT r.res_id, r.resdate, r.checkindate, r.checkoutdate, r.rescancel, r.discountapplied, r.customer_id, r.room_id
from reservations r, services s
WHERE r.res_id=s.res_id and s.typeofservice = userInput1;
-- Create cursor will all reservation elements
ss_row showServ_cursor%rowtype;
-- Assign cursor to variable
counter456 number;
--counter to check reservations existance
BEGIN
counter456 := 0; 
-- assign default value of 0 to counter
FOR ss_row IN showServ_cursor
LOOP

Dbms_output.put_line('Reservation ID:'|| ss_row.res_id||'  Reservation Date '|| 
ss_row.resdate||'  Check In Date:  '||ss_row.checkindate||'  Check Out Date  ' ||ss_row.checkoutdate || 
'  Reservation Cancelled? '|| ss_row.rescancel||'   Discount Applied?  '|| ss_row.discountapplied||' Customer ID  '||ss_row.customer_id|| '  Room ID'||ss_row.room_id);
-- iterate over every row of cursor and output info
counter456 := counter456 + 1;
--add one to counter for every row in cursor
END LOOP;
IF ( counter456 = 0 ) THEN
    Dbms_output.put_line('There are no reservations that have that service');
END IF;
--if counter is still at default value then for loop was never engaged, meaning the input doesnt exist
END;

-- 18)
CREATE OR REPLACE PROCEDURE TotalServiceIncomeRep (hotelID in number)IS CURSOR TotalIncome_cursor IS
select i.laundrycost, i.res_id
from invoices i, reservations r, rooms ro, hotelbranch hb
Where i.res_id = r.res_id AND
r.room_id = ro.room_id AND
ro.hotelbranch_id = hb.hotelbranch_id AND
hb.hotelbranch_id = HotelID;

-- Create cursor will service invoice information
-- Joined Res_id, room_id, hotelbranch_id


Hotelincome_row TotalIncome_cursor%rowtype;
--assign cursor to a variable
Counter123 number;
--counter for total service income
CounterHotel number;
--counter to check if hotel input is valid
BEGIN
Counter123 := 0;
CounterHotel := 0;
-- counters set to 0
FOR Hotelincome_row IN TotalIncome_cursor
--iterate over cursor
LOOP
Counter123 := counter123 + Hotelincome_row.laundrycost;
-- counter equals itself plus cursor's current laundrycost value for every row of cursor
CounterHotel := CounterHotel + 1;
END LOOP;
IF ( CounterHotel = 0 ) THEN
    Dbms_output.put_line('Invalid Hotel Input');
    --if counter is still at default value then for loop was never engaged, meaning the input doesnt exist
ELSE
Dbms_output.put_line('The total service income received at hotel ' || hotelID|| ' is: ' || counter123);
-- Output
END IF;
END;






--END OF MEMBER 4 PROCEDURES--
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
--MEMBER 5 PROCEDURES--
--Name: Gavin Rader
--------------------------------------------------------------------------------------------------------------------------------
-- 19) 
Set Serveroutput ON;
--Creation of Procedure
create or replace procedure ShowAvlRooms (HotelID IN Number)AS
 
hotel_row hotel%rowtype;
hotel_rooms NUMBER;
hotel_singles NUMBER;
hotel_doubles NUMBER;
hotel_suites NUMBER;
hotel_conf NUMBER;
invalidID EXCEPTION;
 
 
begin
if HotelID <= 0 then raise invalidID;
else
select count(roomID) into hotel_rooms from rooms
where HotelID = rooms.hotelbranchID;
dbms_output.put_line('Total rooms located in this hotel: ' || hotel_rooms);

select count(roomID) into hotel_singles from rooms
where HotelID = rooms.hotelbranchID AND roomType = 'Single';
dbms_output.put_line('Total single rooms located in this hotel: ' || hotel_singles);

select count(roomID) into hotel_doubles from rooms
where HotelID = rooms.hotelbranchID AND roomType = 'Double';
dbms_output.put_line('Total double rooms located in this hotel: ' || hotel_doubles);

select count(roomID) into hotel_suites from rooms
where HotelID = rooms.hotelbranchID AND roomType = 'Suite';
dbms_output.put_line('Total double rooms located in this hotel: ' || hotel_suites);

select count(roomID) into hotel_conf from rooms
where HotelID = rooms.hotelbranchID AND roomType = 'Conference';
dbms_output.put_line('Total double rooms located in this hotel: ' || hotel_conf);
end if;

Exception
when no_data_found then
	dbms_output.put_line('No Rooms available'); 
when invalidID then
dbms_output.put_line('Enter a Hotel ID greater then 0.');
end;

exec ShowAvlRooms(2);

--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
--20) 
Set Serveroutput ON;
--Creation of Procedure and Cursor
create or replace procedure RoomCheckoutReport (ReservationID IN Number)IS
cursor checkout is
--Select statement of required variables
Select firstName, lastName, roomID, roomRate, discountApplied, InvoiceTotal
from Customer, Reservations, Invoice
--Error cheking to make sure that only entries are shown from the ID input
where ReservationID = reservations.resID AND reservations.resid = invoice.resid AND Customer.customerid = invoice.customerid;
checkout_row reservations%rowtype;

begin
for checkout_row in checkout
--Loop that will continue to show all reports for given reservation ID.
loop
--Header that sits at the top of report
dbms_output.put_line('----------------------------------------------------');
dbms_output.put_line('Room checkout Report:');
dbms_output.put_line('First Name: ' || checkout_row.firstName);
dbms_output.put_line('Last Name: ' || checkout_row.lastName);
dbms_output.put_line('Room Number: ' || checkout_row.roomID);
dbms_output.put_line('Daily Room Rate: $' || checkout_row.roomRate);
dbms_output.put_line('Discount Applied: $' || checkout_row.discountApplied);
dbms_output.put_line('Invoice Total: $' || checkout_row.InvoiceTotal);
dbms_output.put_line('----------------------------------------------------');
end loop;
end;

Exec RoomCheckoutReport(1);

--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
--21) 
Set Serveroutput ON;
--Creation of Procedure and Cursor
create or replace procedure IncomeByStateReport (stateID IN VARCHAR)IS
cursor stateReport is
--Select statement of required variables
Select state_, invoiceID, laundryCost, realCost, roomCost, InvoiceTotal
from Hotel, Invoice
Where hotel.state_ = stateID AND hotel.hotelbranchID = invoice.hotelBranchID;
stateReport_row invoice%rowtype;

begin
dbms_output.put_line('Income By State Report:');
dbms_output.put_line('State: ' || stateID);
for stateReport_row in stateReport
--Loop that will continue to show all reports for given State Initials.
loop
--Header that sits at the top of report
dbms_output.put_line('--------------------------------------------------------------');
dbms_output.put_line('Invoice: ' || stateReport_row.invoiceID);
dbms_output.put_line('Laundry Cost: ' || stateReport_row.laundryCost);
dbms_output.put_line('Restraunt Cost: ' || stateReport_row.realCost);
dbms_output.put_line('Room Cost: ' || stateReport_row.roomCost);
dbms_output.put_line('Total Income from invoice: ' || stateReport_row.InvoiceTotal);
dbms_output.put_line('--------------------------------------------------------------');

end loop;
end;

Exec IncomeByStateReport('MD');

--------------------------------------------------------------------------------------------------------------------------------
--END OF MEMBER 5 PROCEDURES--
---------------------------------------------------------------------------------------



