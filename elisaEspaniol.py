#-eliza en español
# rafael traduccion del eliza original en python
#----------------------------------------------------------------------

import string
import re
import random

class eliza:
  def __init__(self):
    self.keys = map(lambda x:re.compile(x[0], re.IGNORECASE),gPats)
    self.values = map(lambda x:x[1],gPats)

  #----------------------------------------------------------------------
  # translate: take a string, replace any words found in dict.keys()
  #  with the corresponding dict.values()
  #----------------------------------------------------------------------
  def translate(self,str,dict):
    words = string.split(string.lower(str))
    keys = dict.keys();
    for i in range(0,len(words)):
      if words[i] in keys:
        words[i] = dict[words[i]]
    return string.join(words)

  #----------------------------------------------------------------------
  #  respond: take a string, a set of regexps, and a corresponding
  #    set of response lists; find a match, and return a randomly
  #    chosen response from the corresponding list.
  #----------------------------------------------------------------------
  def respond(self,str):
    # find a match among keys
    for i in range(0,len(self.keys)):
      match = self.keys[i].match(str)
      if match:
        # found a match ... stuff with corresponding value
        # chosen randomly from among the available options
        resp = random.choice(self.values[i])
        # we've got a response... stuff in reflected text where indicated
        pos = string.find(resp,'%')
        while pos > -1:
          num = string.atoi(resp[pos+1:pos+2])
          resp = resp[:pos] + \
            self.translate(match.group(num),gReflections) + \
            resp[pos+2:]
          pos = string.find(resp,'%')
        # fix munged punctuation at the end
        if resp[-2:] == '?.': resp = resp[:-2] + '.'
        if resp[-2:] == '??': resp = resp[:-2] + '?'
        return resp

#----------------------------------------------------------------------
# gReflections, a translation table used to convert things you say
#    into things the computer says back, e.g. "I am" --> "you are"
#----------------------------------------------------------------------
gReflections = {
  "soy"   : "eres",
  "era"  : "eras",
  "yo"    : "tu",
  "habría"  : "habrías",
  "he"  : "has",
  "en el futuro"  : "en el futuro",
  "mi"  : "tu",
  "eres"  : "soy",
  "has": "he",
  "as": "e",
  "tu"  : "mi",
  "tuyo"  : "mio",
  "tu"  : "yo",
  "yo"  : "tu"
}

#----------------------------------------------------------------------
# gPats, the main response table.  Each element of the list is a
#  two-element list; the first is a regexp, and the second is a
#  list of possible responses, with group-macros labelled as
#  %1, %2, etc.  
#----------------------------------------------------------------------
gPats = [
  [r'Necesito (.*)',
  [  "Para que necesitas %1?",
    "De verdad necesitas ayuda para conseguir %1?",
    "Seguro que necesitas %1?"]],
  
  [r'Por que no te ([^\?]*)\??',
  [  "Crees que no me %1?",
    "Quizás finalmente me %1.",
    "De verdad quieres que me %1?"]],
  
  [r'Por que no puedo ([^\?]*)\??',
  [  "Crees que deberías poder %1?",
    "Si pudieras %1, que harías?",
    "No se -- porque no puedes %1?",
    "Has probado de verdad?"]],
  
  [r'No puedo (.*)',
  [  "Como sabes que no puedes %1?",
    "Quizá podrías %1 si quisieras.",
    "Que necesitarías para %1?"]],
  
  [r'Estoy (.*)',
  [  "Hablas conmigo porque estás %1?",
    "Cuanto tiempo has estado %1?",
    "Como se siente el estar %1?"]],
  
  [r'Soy (.*)',
  [  "Como te hace sentir ser %1?",
    "Disfrutas ser %1?",
    "Por que me dices que eres %1?",
    "Por que crees que eres %1?"]],
  
  [r'Eres ([^\?]*)\??',
  [  "Importa si soy %1?",
    "Preferirías que no fuera %1?",
    "Quizá creas que soy %1.",
    "Quizá deba ser %1 -- que crees?"]],
  
  [r'Que (.*)',
  [  "Por que lo preguntas?",
    "Como te ayudaría la respuesta?",
    "Tu que crees?"]],
  
  [r'Como (.*)',
  [  "Como crees?",
    "Quizá puedas responder tu propia pregunta.",
    "Que quieres preguntar en realidad?"]],
  
  [r'Porque (.*)',
  [  "Es esa la razón real?",
    "Que otros motivos te vienen a la mente?",
    "Ese motivo se aplica a todo?",
    "Que se deduce de %1?"]],
  
  [r'(.*) perdón (.*)',
  [  "Hay momentos cuando las disculpas no son necesarias.",
    "Que sentimientos tienes al disculparte?"]],
  
  [r'Hola(.*)',
  [  "Hola... Me alegro de que te pases hoy.",
    "Hola... que tal se está por ahí?",
    "Hola, como te sientes hoy?"]],
  
  [r'Creo que (.*)',
  [  "Dudas que %1?",
    "De verdad lo crees?",
    "No estás seguro de que %1?"]],
  
  [r'(.*) amig[oa] (.*)',
  [  "Cuentame más de tus amigos.",
    "Cuando piensas en un amigo que te viene a la cabeza?",
    "Porque no me hablas de un amigo de la infancia?"]],
  
  [r'S[ií]',
  [  "Pareces muy seguro.",
    "Vale, pero puedes explicar por que?"]],
  
  [r'(.*) ordenador(.*)',
  [  "De verdad me estás hablando a mí?",
    "Se siente raro el hablar con un ordenador?",
    "Como te hacen sentir los ordenadores?",
    "Te sientes amenazado por los ordenadores?"]],
  
  [r'es (.*)',
  [  "Crees que es %1?",
    "Quizá sea %1 -- que crees?",
    "Que harías si fuera %1?",
    "Podría ser %1.",
    "Pareces muy seguro.",
    "Como te sentirías si te dijera que no es %1?"]],
  
  [r'Puedes ([^\?]*)\??',
  [  "Que te hace pensar que no puedo %1?",
    "Si pudiera %1, entonces que?",
    "Por que preguntas si puedo %1?"]],
  
  [r'Puedo ([^\?]*)\??',
  [  "Quizá no quieras %1.",
    "Quieres ser capaz de %1?",
    "Si pudieras %1, lo harías?"]],
  
  [r'Eres (.*)',
  [  "Por que crees que soy %1?",
    "Te agrada pensar que soy %1?",
    "Quizá quieres que sea %1.",
    "Quizá estes hablando sobre ti?",
    "Por que dices que soy %1?",
    "Estás hablando sobre ti o sobre mi?"]],
  
  [r'No (.*)',
  [  "No has %1?",
    "Por que no has %1?",
    "Quieres %1?"]],
  
  [r'Me siento (.*)',
  [  "Bien, hablame más de esos sentimientos.",
    "Te sientes %1 frecuentemente?",
    "Cuando te sueles sentir %1?",
    "Cuando te sientes %1, que haces?"]],
  
  [r'Tengo (.*)',
  [  "Porque me dices que tienes %1?",
    "Realmente tienes %1?",
    "Ahora que tienes %1, que será lo próximo?"]],
  
  [r'Debería (.*)',
  [  "Podrías explicar porque deberías %1?",
    "Por que deberías %1?",
    "Quien mas sabe eso %1?"]],
  
  [r'Hay (.*)',
  [  "Crees que hay %1?",
    "Sí, parece que hay %1.",
    "Te gusta que haya %1?"]],
  
  [r'Mi (.*)',
  [  "Ya veo, tu %1.",
    "Por que dirías que tu %1?",
    "Cuando tu %1, que crees?"]],

  [r'Tu (.*)',
  [  "Deberíamos hablar sobre ti, no sobre mi.",
    "Por que hablas sobre mí?",
    "Por que te importa si yo %1?"]],
    
  [r'Por que (.*)',
  [  "Por que no me cuentas de que %1?",
    "Por que piensas que %1?" ]],
    
  [r'Quiero (.*)',
  [  "Que significaría que tuvieras %1?",
    "Porque quieres %1?",
    "Que harías si tuvieras %1?",
    "Si tuvieras %1, que harías?"]],
  
  [r'(.*) madre(.*)',
  [  "Hablame más sobre tu madre.",
    "Como era la relación con tu madre?",
    "Como se relaciona con tus sentimientos actuales?",
    "Las buenas relaciones de familia son importantes."]],
  
  [r'(.*) padre(.*)',
  [  "Hablame más sobre tu padre.",
    "Como era la relación con tu padre?",
   "Tienen alguna relación la relación con tu padre con tus sentimientos actuales?",
    "Tienes problemas mostrando afecto hacia tu familia?"]],

  [r'(.*) niño(.*)',
  [  "Tenías amigos cercanos de niño?",
    "Cual es tu memoria preferida de la infancia?",
    "Recuerdas algún sueño o pesadilla de la infancia?",
    "Te molestaban a veces los otros niños?",
    "Como crees que se relacionan tus experiencias de la infancia con tus sentimientos actuales?"]],
    
  [r'(.*)\?',
  [  "Por que preguntas eso?",
    "Por favor, considera si puedes responder tu propia pregunta.",
    "Quizá la pregunta esté dentro de ti?"]],
  
  [r'salir',
  [  "Gracias por hablar conmigo.",
    "Adios.",
    "Gracias, serán $150. Ten un buen día!"]],
  
  [r'(.*)',
  [  "Por favor cuentame más.",
    "Vamos a cambiar un poco de tema... Hablame sobre tu familia.",
    "Puedes explicar eso?",
    "Por que dices %1?",
    "Ya veo.",
    "Muy interesante.",
    "%1.",
    "Ya veo. Y que te dice eso?",
    "Como te hace sentir eso?",
    "Como te sientes cuando dices eso?"]]
  ]

#----------------------------------------------------------------------
#  command_interface
#----------------------------------------------------------------------
def command_interface():
  print "Terapeuta\n---------"
  print "Habla con el programa en castellano normal, usando letras mayúsculas,-"
  print ' mińusculas y puntuación.  Introduce "salir" cuando hayas acabado.'
  print '='*72
  print "Hola, que tal estas?"
  s = ""
  therapist = eliza();
  while s != "salir":
    try: s = raw_input(">")
    except EOFError:
      s = "salir"
      print s
    while s[-1] in "!.": s = s[:-1]
    print therapist.respond(s)


if __name__ == "__main__":
  command_interface()
