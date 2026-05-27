* Crear los 5 bloques de eventos intermedios
g event_blanking= urgencias3m_blank==1 & ///
((causar_ingrehosp13m_blank_3m==0 | causar_ingrehosp13m_blank_3m==1 | causar_ingrehosp13m_blank_3m==2) & fec_ingrehosp13m_blank_3m< (fec_ablac+90) & fec_ingrehosp13m_blank_3m<.) | ///
((causar_ingrehosp23m_blank_3m==0 | causar_ingrehosp23m_blank_3m==1) & fec_ingrehosp23m_blank_3m< (fec_ablac+90) & fec_ingrehosp23m_blank_3m<.) | ///
((causar_ingrehosp33m_blank_3m==0 | causar_ingrehosp33m_blank_3m==1) & fec_ingrehosp33m_blank_3m< (fec_ablac+90) & fec_ingrehosp33m_blank_3m<.)

g event_ingresos_3m= urgencias3m_event==1 & ///
((causar_ingrehosp13m_event_3m==0 | causar_ingrehosp13m_event_3m==1) & fec_ingrehosp13m_event_3m< (fec_ablac+90) & fec_ingrehosp13m_event_3m<.) | ///
((causar_ingrehosp23m_event_3m==0 | causar_ingrehosp23m_event_3m==1) & fec_ingrehosp23m_event_3m< (fec_ablac+90) & fec_ingrehosp23m_event_3m<.) | ///
((causar_ingrehosp33m_event_3m==0 | causar_ingrehosp33m_event_3m==1) & fec_ingrehosp33m_event_3m< (fec_ablac+90) & fec_ingrehosp33m_event_3m<.)

g event_persist_3m= persist73m_event_3m==1 & ecgpersist73m_event_3m==1 & (fecpersist73m_event_3m<(fec_ablac+90)) & fecpersist73m_event_3m<.

g event_parox_3m= parox73m_event_3m==1 & ecgparox73m_event_3m==1 & (fecparox73m_event_3m<(fec_ablac+90)) & fecparox73m_event_3m<.

g event_holter_3m = (ritmo_3m==1 | ritmo_3m==2 | ritmo_3m==3 | ritmo_3m==4) & fec_visita_3m<(fec_aleat+90) & fec_visita_3m<.

* Crear la variable resumen
egen event_blank=anymatch(event_blanking event_ingresos_3m event_persist_3m event_parox_3m event_holter_3m), val(1)

* Ver el resultado en pantalla
tab event_blank
