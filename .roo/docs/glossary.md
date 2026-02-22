# Projekt-Glossar: SAP Agent Factory

## System-Landschaft
- **Entwicklungssystem (A4H):** Lokales SAP-System (z.B. SAP NW AS ABAP 7.5x oder ABAP Platform).
- **Mandant (Client):** Standardmäßig Mandant 100 (falls nicht anders in `config.yaml` angegeben).
- **Basis-URL OData:** `http://localhost:50000/sap/opu/odata/sap/`

## Projekt-Struktur
- `/src`: Enthält die ABAP-Quelltexte (Exported via abapGit).
- `/scripts`: Enthält Python-Tools (z.B. `export_sflight_json.py`) für den OData-Export.
- `sflight_data.json`: Die zentrale Export-Datei für Analysen.

## Wichtige Konfigurationen
- **Auth:** Nutzt Umgebungsvariablen (`A4H_USER`, `A4H_PASSWORD`), die über eine `.env`-Datei geladen werden.
- **Transformationslogik:** SAP-Datumsformate (YYYYMMDD) werden im Export zu ISO-Format (YYYY-MM-DD) konvertiert.

## Fachliche Begriffe
- **SFLIGHT:** Standard-Flugdatenmodell von SAP, dient hier als Test-Datensatz für die Agent-Factory.
- **FlightSet:** Das OData-EntitySet für den Zugriff auf Flugdaten.