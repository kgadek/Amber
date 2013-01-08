-include("common.hrl").

-ifndef(LOCALIZATION_DATA_H).
-define(LOCALIZATION_DATA_H, true).

%% Teraz to de facto kopia localizationdata, ale umożliwiamy sobie zmianę
%% szczegółów protokołu kiedyś.
-record(localization, {xpos     :: float(),
                       ypos     :: float(),
                       zpos     :: float(),
                       angle    :: float(),
                       markerid :: uint32()}).

-endif.