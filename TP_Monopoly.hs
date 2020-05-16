import Text.Show.Functions()
import Data.List (genericLength)

data Participante = ConstructorParticipante {
    nombre :: String,
    cantidadDeDinero :: Int,
    tacticaDeJuego :: Tactica,
    propiedadesCompradas :: [Propiedad],
    accionesHechas :: [Accion]
} deriving (Show)


type Tactica = String
type Propiedad = (String, Int)
type Accion = (Participante -> Participante)


carolina :: Participante
carolina = ConstructorParticipante "Carolina" 500    "Accionista"     [] [pagarAAccionista, pasarPorElBanco]
manuel :: Participante
manuel   = ConstructorParticipante  "Manuel"  500 "Oferente Singular" [] [    enojarse    , pasarPorElBanco]


pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = (cambiarDinero (+ 40) . cambiarTactica "Comprador Compulsivo") unParticipante


enojarse :: Accion
enojarse        unParticipante = ( gritar . cambiarDinero (+50)) unParticipante


gritar :: Accion
gritar unParticipante = ((agregarAccion' gritar).cambiarNombre ("AHHHHH" ++)) unParticipante


subastar :: Propiedad -> Accion
subastar unaPropiedad unParticipante 
    | esAccionistaOOferenteSingular unParticipante && leAlcanzaParaComprar unaPropiedad unParticipante  =
         (agregarPropiedad unaPropiedad . cambiarDinero (+ ( - (precioPropiedad unaPropiedad)))) unParticipante
    | esAccionistaOOferenteSingular unParticipante && not (leAlcanzaParaComprar unaPropiedad unParticipante) = hacerBerrinchePor unaPropiedad unParticipante
    | otherwise                                     = unParticipante


hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unParticipante = (subastar unaPropiedad .(gritar . cambiarDinero ( + 10 ))) unParticipante


cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = ((cambiarDinero ( + ((cantidadPropiedadesCaras unParticipante) * 30))) . (cambiarDinero ( + ((cantidadPropiedadesBaratas unParticipante) * 20))))  unParticipante



pagarAAccionista :: Accion
pagarAAccionista unParticipante 
    | esAccionista unParticipante = cambiarDinero  ( + 50 )    unParticipante
    | otherwise                   = cambiarDinero (+ (- 100) ) unParticipante



esAccionistaOOferenteSingular :: Participante -> Bool
esAccionistaOOferenteSingular  (ConstructorParticipante _ _ tacticaDeJuego _ _ ) = elem tacticaDeJuego ["Accionista", "Oferente Singular"]


esAccionistaOOferenteSingular' :: Participante -> Bool
esAccionistaOOferenteSingular' (ConstructorParticipante _ _  "Accionista" _ _ )       = True
esAccionistaOOferenteSingular' (ConstructorParticipante _ _ "Oferente Singular" _ _ ) = True
esAccionistaOOferenteSingular' _                                                      = False


esAccionista                  :: Participante -> Bool
esAccionista (ConstructorParticipante _ _ tacticaDeJuego _ _ )                   = elem tacticaDeJuego ["Accionista"]


esOferenteSingular            :: Participante -> Bool
esOferenteSingular (ConstructorParticipante _ _ tacticaDeJuego _ _ )             = elem tacticaDeJuego ["Oferente Singular"]


cambiarNombre    :: (String -> String) -> Accion
cambiarNombre unaFuncion unParticipante      = unParticipante  { nombre               = unaFuncion       (nombre unParticipante)     }


cambiarDinero    :: (Int -> Int) -> Accion
cambiarDinero unaFuncion unParticipante      = unParticipante  { cantidadDeDinero     = unaFuncion (cantidadDeDinero unParticipante) }


cambiarTactica   :: String -> Accion
cambiarTactica unaTactica unParticipante     = unParticipante  { tacticaDeJuego       =                 unaTactica                   }


agregarPropiedad :: Propiedad -> Participante -> Participante
agregarPropiedad unaPropiedad unParticipante = unParticipante  { propiedadesCompradas = ((++ [(unaPropiedad)]) . propiedadesCompradas) unParticipante }


agregarAccion'   :: (Accion) -> Participante -> Participante
agregarAccion'   unaAccion    unParticipante = unParticipante  { accionesHechas       =    ((++ [unaAccion])   .  accionesHechas)      unParticipante }


esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = ( (< 150) . precioPropiedad) unaPropiedad


esPropiedadCara :: Propiedad -> Bool
esPropiedadCara   unaPropiedad = ( (>= 150) . precioPropiedad) unaPropiedad


precioPropiedad :: Propiedad -> Int
precioPropiedad (_, precio) = precio


cantidadPropiedadesBaratas :: Participante -> Int
cantidadPropiedadesBaratas unParticipante = (length . filter esPropiedadBarata) (propiedadesCompradas unParticipante)

cantidadPropiedadesCaras :: Participante -> Int
cantidadPropiedadesCaras unParticipante   = (length . filter esPropiedadCara)   (propiedadesCompradas unParticipante)


leAlcanzaParaComprar :: Propiedad -> Participante -> Bool
leAlcanzaParaComprar unaPropiedad unParticipante = (cantidadDeDinero unParticipante) >= precioPropiedad unaPropiedad
{-
gritar' :: Accion
gritar' unParticipante = unParticipante { nombre = (("AHHHHH" ++) . nombre) unParticipante }


subastar' :: Propiedad -> Accion
subastar' unaPropiedad unParticipante 
    | esAccionistaOOferenteSingular  unParticipante && (cantidadDeDinero unParticipante) >= precioPropiedad unaPropiedad =
         (agregarPropiedad unaPropiedad . cambiarDinero (+ ( - (precioPropiedad unaPropiedad)))) unParticipante
    | otherwise                                     = unParticipante
    

pagarAAccionista' :: (Participante -> Bool) -> Accion
pagarAAccionista' esAccionista        unParticipante = cambiarDinero  ( + 50 )    unParticipante
pagarAAccionista' not (esAccionista)  unParticipante = cambiarDinero (+ (- 100) ) unParticipante


tipoPropiedad :: Propiedad -> Bool
tipoPropiedad unaPropiedad unaFuncion = (unaFuncion . precioPropiedad) unaPropiedad


cantidadTiposDePropiedades :: Participante -> Int
cantidadTiposDePropiedades unParticipante = ((length . filter (tipoPropiedad (< 150)) ) . (length . filter (tipoPropiedad (>=150)) )) ( propiedadesCompradas unParticipante)
-}