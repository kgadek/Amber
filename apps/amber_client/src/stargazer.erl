-module(stargazer).

-include("include/drivermsg_pb.hrl").
-include("include/stargazer_pb.hrl").
-include("include/common.hrl").
-include("include/localization_data.hrl").

-export([stargazer_order_position/0, stargazer_order_position/1,
         stargazer_get_position/1, stargazer_get_position/2,
         stargazer_subscribe_position/1, stargazer_subscribe_position/2]).


%% @equiv stargazer_order_position(self())
stargazer_order_position() -> stargazer_order_position(self()).


%% @doc Zgłasza żądanie pobrania położenia. Odpowiedź będzie wysłana do procesu
%% podanego poprzez parametr pid w wiadomości typu #amber_client_msg{}.
%% 
%% Zaleca się wykorzystanie funkcji {@link stargazer_get_position/1} do
%% odbierania tych wiadomości.
-type stargazer_order_position_future_ref() :: non_neg_integer().
-spec stargazer_order_position(pid())
      -> stargazer_order_position_future_ref().
stargazer_order_position(Pid) ->
  SynNum = amber_client:get_synnum(),
  MsgBase = #drivermsg{type = 'DATA', synnum = SynNum},
  {ok, Msg} = stargazer_pb:set_extension(MsgBase, datarequest, #datarequest{}),
  MsgBinary = stargazer_pb:encode_drivermsg(Msg),
  DevT = amber_client:env(stargazer_devt),
  DevI = amber_client:env(stargazer_devi),
  Hdr = #driverhdr{devicetype = DevT, deviceid = DevI},
  Key = #dispd_key{dev_t=DevT, dev_i=DevI, synnum=SynNum},
  Val = #dispd_val{recpid = Pid, post = {fun amber_client:deregister_receiver/1, Key}},
  amber_client:register_receiver(Key, Val),
  amber_client:send_to_amber(Hdr, MsgBinary),
  SynNum.



%% @equiv stargazer_subscribe_position(Freq, self())
stargazer_subscribe_position(Freq) -> stargazer_subscribe_position(Freq, self()).


%% @doc Wywołanie asynchroniczne. Zwracana wartość -- SynNum -- jest numerem
%% żądania cyklicznego.
%% 
%% W odpowiedzi na żądanie cykliczne dany proces będzie otrzymywać wiadomości
%% typu `#amber_client_msg{}' co 100ms.
%% 
%% Parametry są takie same jak dla funkcji {@link stargazer_order_position/1}.
stargazer_subscribe_position(Freq, Pid) ->
  SynNum = 0, % tego wymaga sterownik stargazera
  MsgBase = #drivermsg{type = 'DATA', synnum = SynNum},
  {ok, Msg} = stargazer_pb:set_extension(MsgBase, subscribeaction,
                                         #subscribeaction{action = 'SUBSCRIBE',
                                                          freq   = Freq }),
  DevT = amber_client:env(stargazer_devt),
  DevI = amber_client:env(stargazer_devi),
  Hdr = #driverhdr{devicetype = DevT, deviceid = DevI},
  MsgBinary = stargazer_pb:encode_drivermsg(Msg),
  Key = #dispd_key{dev_t=DevT, dev_i=DevI, synnum=SynNum},
  Val = #dispd_val{recpid = Pid},
  amber_client:register_receiver(Key, Val),
  amber_client:send_to_amber(Hdr, MsgBinary),
  SynNum.



%% @equiv stargazer_get_position(SynNum, 5000)
stargazer_get_position(SynNum) -> stargazer_get_position(SynNum, 5000).


%% @doc Zwraca położenie. SynNum jest numerem żądania zwrócanym przez funkcje
%% {@link stargazer_order_position/0}. Timeout wyznacza maksymalny czas
%% oczekiwania -- w wypadku jego przekroczenia wywoływana jest funkcja
%% `erlang:error(stargazer_get_position_timeout)'.
%% 
%% Odpowiedź na żądanie jest wysyłana do danego procesu jako wiadomość typu
%% `#amber_client_msg{}'. Klient może chcieć samemu je odbierać. W takim wypadku
%% należy użyć funkcji
%% `stargazer_drivermsg_to_location(ACM#amber_client_msg.msg)' na odebranej
%% wiadomości ACM.
-spec stargazer_get_position(stargazer_order_position_future_ref(), timeout())
      -> #localization{}.
stargazer_get_position(SynNum, Timeout) ->
  DevT = amber_client:env(stargazer_devt),
  DevI = amber_client:env(stargazer_devi),
  receive #amber_client_msg{hdr = #driverhdr{devicetype=DevT, deviceid=DevI},
                            msg = #drivermsg{synnum=SynNum} = Msg} ->
    stargazer_drivermsg_to_location(Msg)
  after Timeout -> error(stargazer_get_position_timeout)
  end.


%% @doc Wyciąga dane lokalizacyjne z wiadomości.
-spec stargazer_drivermsg_to_location(#drivermsg{})
      -> #localization{}.
stargazer_drivermsg_to_location(Msg) ->
  DMsg = stargazer_pb:decode_extensions(Msg),
  {ok, #localizationdata{xpos=X,ypos=Y,zpos=Z,angle=A,markerid=M,timestamp=_T}}
              = stargazer_pb:get_extension(DMsg, localizationdata),
  #localization{xpos=X, ypos=Y, zpos=Z, angle=A, markerid=M}.
